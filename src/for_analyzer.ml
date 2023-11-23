open! Core
module Cutoff = Cutoff.For_analyzer
module Internal_node_id = Node_id

module Kind = struct
  type t =
    | Array_fold
    | At of { at : Time_ns.Alternate_sexp.t }
    | At_intervals of
        { base : Time_ns.Alternate_sexp.t
        ; interval : Time_ns.Span.t
        }
    | Bind_lhs_change
    | Bind_main
    | Const
    | Expert
    | Freeze
    | If_test_change
    | If_then_else
    | Invalid
    | Join_lhs_change
    | Join_main
    | Map
    | Snapshot of { at : Time_ns.Alternate_sexp.t }
    | Step_function
    | Uninitialized
    | Unordered_array_fold
    | Var
    | Map2
    | Map3
    | Map4
    | Map5
    | Map6
    | Map7
    | Map8
    | Map9
    | Map10
    | Map11
    | Map12
    | Map13
    | Map14
    | Map15
  [@@deriving sexp]

  let to_string t = Sexp.to_string ([%sexp_of: t] t)
end

let kind (Node.Packed.T node) : Kind.t =
  match node.kind with
  | Array_fold _ -> Array_fold
  | At { at; _ } -> At { at }
  | At_intervals { base; interval; _ } -> At_intervals { base; interval }
  | Bind_lhs_change _ -> Bind_lhs_change
  | Bind_main _ -> Bind_main
  | Const _ -> Const
  | Expert _ -> Expert
  | Freeze _ -> Freeze
  | If_test_change _ -> If_test_change
  | If_then_else _ -> If_then_else
  | Invalid -> Invalid
  | Join_lhs_change _ -> Join_lhs_change
  | Join_main _ -> Join_main
  | Map _ -> Map
  | Snapshot { at; _ } -> Snapshot { at }
  | Step_function _ -> Step_function
  | Uninitialized -> Uninitialized
  | Unordered_array_fold _ -> Unordered_array_fold
  | Var _ -> Var
  | Map2 _ -> Map2
  | Map3 _ -> Map3
  | Map4 _ -> Map4
  | Map5 _ -> Map5
  | Map6 _ -> Map6
  | Map7 _ -> Map7
  | Map8 _ -> Map8
  | Map9 _ -> Map9
  | Map10 _ -> Map10
  | Map11 _ -> Map11
  | Map12 _ -> Map12
  | Map13 _ -> Map13
  | Map14 _ -> Map14
  | Map15 _ -> Map15
;;

module Dot_user_info = struct
  include Dot_user_info

  let default ~name ~kind ~height =
    let label =
      [ name; Sexp.to_string ([%sexp_of: Kind.t] kind); sprintf "height=%d" height ]
    in
    Dot_user_info.dot ~label ~attributes:String.Map.empty
  ;;
end

module Node_id = Int

module Stabilization_num = struct
  include Stabilization_num
  include Stabilization_num.For_analyzer
end

let recomputed_at (Node.Packed.T node) = node.recomputed_at
let changed_at (Node.Packed.T node) = node.changed_at
let node_id (Node.Packed.T node) = Internal_node_id.to_string node.id |> Node_id.of_string
let cutoff (Node.Packed.T node) = Cutoff.of_cutoff node.cutoff
let user_info (Node.Packed.T node) = node.user_info
let height (Node.Packed.T node) = node.height
let iteri_children (Node.Packed.T node) = Node.iteri_children node

let maybe_iter_on_bind_nodes_created_on_rhs (Node.Packed.T node) ~f =
  match node.kind with
  | Bind_lhs_change bind -> Bind.iter_nodes_created_on_rhs bind ~f
  | _ -> ()
;;

let directly_observed = State.directly_observed

let traverse packed_list ~add_node =
  let map_of_iter iterator ~f =
    let out = ref [] in
    iterator ~f:(fun x -> out := f x :: !out);
    List.rev !out
  in
  Node.Packed.iter_descendants packed_list ~f:(fun packed_node ->
    let children =
      map_of_iter
        (fun ~f -> iteri_children packed_node ~f:(fun _ node -> f node))
        ~f:node_id
    in
    let bind_children =
      map_of_iter (maybe_iter_on_bind_nodes_created_on_rhs packed_node) ~f:node_id
    in
    let id = node_id packed_node in
    let kind = kind packed_node in
    let cutoff = cutoff packed_node in
    let user_info = user_info packed_node in
    let recomputed_at = recomputed_at packed_node in
    let changed_at = changed_at packed_node in
    let height = height packed_node in
    add_node
      ~id
      ~kind
      ~cutoff
      ~children
      ~bind_children
      ~user_info
      ~recomputed_at
      ~changed_at
      ~height)
;;
