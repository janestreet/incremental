open! Core
module Node_id = Incremental.For_analyzer.Node_id
module Dot_user_info = Incremental.For_analyzer.Dot_user_info

type t =
  { nodes : Node.t list
  ; seen : Node_id.Set.t
  ; num_stabilizes : int
  }
[@@deriving sexp]

module Render_target = struct
  type t =
    | Dot
    | Graph_easy
end

let normalize_ids skeleton =
  let nodes = skeleton.nodes in
  match nodes with
  | [] -> skeleton
  | hd :: _ as nodes ->
    let lowest_id =
      List.map nodes ~f:(fun node -> Node_id.to_int node.id)
      |> List.min_elt ~compare:Int.compare
      |> Option.value ~default:(Node_id.to_int hd.id)
    in
    let new_id node_id =
      (* Nodes are one-indexed *)
      Node_id.to_int node_id - lowest_id + 1 |> Node_id.of_int
    in
    let nodes =
      List.map nodes ~f:(fun node ->
        let id = new_id node.id in
        let children = List.map node.children ~f:new_id in
        let bind_children = List.map node.bind_children ~f:new_id in
        { node with id; children; bind_children })
    in
    let seen = Node_id.Set.map skeleton.seen ~f:new_id in
    { skeleton with nodes; seen }
;;

let snapshot ?(normalize = false) state =
  let seen = Node_id.Hash_set.create () in
  let nodes = ref Reversed_list.[] in
  let add_node
    ~id
    ~kind
    ~cutoff
    ~children
    ~bind_children
    ~user_info
    ~recomputed_at
    ~changed_at
    ~height
    =
    Hash_set.add seen id;
    let new_node =
      { Node.id
      ; kind
      ; cutoff
      ; children
      ; bind_children
      ; user_info
      ; recomputed_at
      ; changed_at
      ; height
      }
    in
    nodes := new_node :: !nodes
  in
  Incremental.For_analyzer.traverse
    (Incremental.For_analyzer.directly_observed state)
    ~add_node;
  let skeleton =
    { nodes = Reversed_list.rev !nodes
    ; seen = Node_id.Set.of_hash_set seen
    ; num_stabilizes = Incremental.State.num_stabilizes state
    }
  in
  if normalize then normalize_ids skeleton else skeleton
;;

let node_name (node_id : Node_id.t) = "n" ^ Node_id.to_string node_id

let make_node ~(node : Node.t) ~extra_attrs ~(render_target : Render_target.t) =
  let name = node_name node.id in
  let base_node_dot =
    Dot_user_info.dot
      ~label:
        [ name
        ; Sexp.to_string ([%sexp_of: Incremental.For_analyzer.Kind.t] node.kind)
        ; sprintf "height=%d" node.height
        ]
      ~attributes:
        (match render_target with
         | Dot -> String.Map.singleton "fontname" "Sans Serif"
         | Graph_easy -> String.Map.empty)
  in
  let info =
    let node_info =
      Option.value_map node.user_info ~default:base_node_dot ~f:(fun user_info ->
        Dot_user_info.append base_node_dot user_info)
    in
    let attrs_opt = extra_attrs node in
    Option.value_map attrs_opt ~default:node_info ~f:(fun attrs ->
      Dot_user_info.append node_info attrs)
  in
  Text_block.text
    (Dot_user_info.to_string
       ?shape:
         (match render_target with
          | Dot -> None
          | Graph_easy -> Some "box")
       ~name
       (Dot_user_info.to_dot info))
;;

(* The parameters' names reflect the ordering of these nodes in the [Incr] graph where
   the children of a node are the inputs (e.g. a Var would be the child of a Map), but it
   seems more intuitive to visualize it in the opposite direction*)
let edge ~from ~to_ = Text_block.textf {|%s -> %s|} (node_name to_) (node_name from)

let make_edges ~(nodes : Node.t list) ~desired_nodes =
  List.concat_map nodes ~f:(fun from_node ->
    let from_node_id = from_node.id in
    List.filter_map from_node.children ~f:(fun to_node_id ->
      Option.some_if
        (Set.mem desired_nodes to_node_id)
        (edge ~from:from_node_id ~to_:to_node_id)))
;;

let bind_edge ~from ~to_ =
  Text_block.textf {|%s -> %s [style=dashed]|} (node_name to_) (node_name from)
;;

(* Note that the direction of information flow is flipped in bind nodes as compared to
   regular child nodes *)
let make_bind_edges ~(nodes : Node.t list) ~desired_nodes ~seen =
  List.concat_map nodes ~f:(fun to_node ->
    let to_node_id = to_node.id in
    List.filter_map to_node.bind_children ~f:(fun from_node_id ->
      Option.some_if
        (Set.mem desired_nodes from_node_id && Set.mem seen from_node_id)
        (bind_edge ~from:from_node_id ~to_:to_node_id)))
;;

let find_connected_nodes
  ~(start_nodes : Node_id.t list)
  ~(edges : Node_id.t list Node_id.Map.t)
  =
  let rec recurse_node node_id seen =
    if Set.mem seen node_id
    then seen
    else (
      let seen = Set.add seen node_id in
      let node_ids = Map.find_multi edges node_id in
      List.fold node_ids ~init:seen ~f:(fun seen child_id -> recurse_node child_id seen))
  in
  List.fold start_nodes ~init:Node_id.Set.empty ~f:(fun seen node_id ->
    recurse_node node_id seen)
;;

let to_dot
  ?(extra_attrs = fun _ -> None)
  ?(render_target = Render_target.Dot)
  ?(filtered_nodes : Node.t list = [])
  ?(render_relation = Render_relation.All)
  { nodes; seen; num_stabilizes = _ }
  =
  let desired_nodes =
    let child_edges =
      lazy
        (List.fold nodes ~init:Node_id.Map.empty ~f:(fun child_edges node ->
           let added_bind =
             List.fold node.bind_children ~init:child_edges ~f:(fun edges bind_id ->
               Map.add_multi edges ~key:bind_id ~data:node.id)
           in
           List.fold node.children ~init:added_bind ~f:(fun edges child_id ->
             Map.add_multi edges ~key:node.id ~data:child_id)))
    in
    let parent_edges =
      lazy
        (Map.fold
           (force child_edges)
           ~init:Node_id.Map.empty
           ~f:(fun ~key:node_id ~data:children parent_edges ->
             List.fold children ~init:parent_edges ~f:(fun parent_edges child_id ->
               Map.add_multi parent_edges ~key:child_id ~data:node_id)))
    in
    let find_connected_nodes =
      find_connected_nodes ~start_nodes:(List.map filtered_nodes ~f:(fun node -> node.id))
    in
    match render_relation with
    | Ancestors -> find_connected_nodes ~edges:(force child_edges)
    | Descendants -> find_connected_nodes ~edges:(force parent_edges)
    | Both ->
      Set.union
        (find_connected_nodes ~edges:(force parent_edges))
        (find_connected_nodes ~edges:(force child_edges))
    | All -> Node_id.Set.of_list (List.map nodes ~f:(fun node -> node.id))
  in
  let desired_node_list =
    List.filter nodes ~f:(fun node -> Set.mem desired_nodes node.id)
  in
  let edges = make_edges ~nodes:desired_node_list ~desired_nodes in
  let bind_edges = make_bind_edges ~nodes:desired_node_list ~desired_nodes ~seen in
  let nodes =
    List.map desired_node_list ~f:(fun node ->
      make_node ~node ~extra_attrs ~render_target)
  in
  let text_block =
    let open Text_block in
    vcat
      [ text "digraph G {"
      ; indent
          (vcat
             (text "rankdir = TB"
              :: text "bgcolor = transparent"
              :: (nodes @ edges @ bind_edges)))
      ; text "}"
      ]
  in
  Text_block.render text_block
;;
