open Core_kernel
open Import
open Types.Kind

module As_adjust_heights_list = Node.Packed.As_list (struct
    let next (Node.Packed.T node) = node.next_in_adjust_heights_heap
  end)

module Nodes_by_height = struct
  type t = As_adjust_heights_list.t Array.t [@@deriving sexp_of]

  let sexp_of_t t =
    let max_nonempty_index = ref (-1) in
    Array.iteri t ~f:(fun i l -> if Uopt.is_some l then max_nonempty_index := i);
    Array.sub t ~pos:0 ~len:(!max_nonempty_index + 1) |> [%sexp_of: t]
  ;;

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      Array.iteri t ~f:(fun height nodes ->
        As_adjust_heights_list.invariant nodes;
        As_adjust_heights_list.iter nodes ~f:(fun (T node) ->
          assert (node.height_in_adjust_heights_heap = height);
          assert (node.height > node.height_in_adjust_heights_heap);
          if Node.is_in_recompute_heap node
          then
            assert (
              node.height_in_recompute_heap = node.height_in_adjust_heights_heap))))
  ;;

  let create ~max_height_allowed = Array.create ~len:(max_height_allowed + 1) Uopt.none

  let length t =
    let r = ref 0 in
    Array.iter t ~f:(fun node -> r := !r + As_adjust_heights_list.length node);
    !r
  ;;
end

type t = Types.Adjust_heights_heap.t =
  { mutable length : int
  ; mutable height_lower_bound : int
  ; mutable max_height_seen : int
  ; mutable nodes_by_height : Nodes_by_height.t
  }
[@@deriving fields, sexp_of]

let is_empty t = length t = 0
let max_height_allowed t = Array.length t.nodes_by_height - 1

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~length:
        (check (fun length ->
           assert (length = Nodes_by_height.length t.nodes_by_height)))
      ~height_lower_bound:
        (check (fun height_lower_bound ->
           assert (height_lower_bound >= 0);
           assert (height_lower_bound <= Array.length t.nodes_by_height);
           for height = 0 to height_lower_bound - 1 do
             assert (Uopt.is_none t.nodes_by_height.(height))
           done))
      ~max_height_seen:
        (check (fun max_height_seen ->
           assert (max_height_seen >= 0);
           assert (max_height_seen <= max_height_allowed t)))
      ~nodes_by_height:(check Nodes_by_height.invariant))
;;

let create ~max_height_allowed =
  { length = 0
  ; height_lower_bound = max_height_allowed + 1
  ; max_height_seen = 0
  ; nodes_by_height = Nodes_by_height.create ~max_height_allowed
  }
;;

let set_max_height_allowed t max_height_allowed =
  if max_height_allowed < t.max_height_seen
  then
    failwiths
      ~here:[%here]
      "cannot set_max_height_allowed less than the max height already seen"
      (max_height_allowed, `max_height_seen t.max_height_seen)
      [%sexp_of: int * [ `max_height_seen of int ]];
  if debug then assert (is_empty t);
  t.nodes_by_height <- Nodes_by_height.create ~max_height_allowed
;;

let add_unless_mem (type a) t (node : a Node.t) =
  if node.height_in_adjust_heights_heap = -1
  then (
    let height = node.height in
    (* We process nodes in increasing order of pre-adjusted height, so it is a bug if we
       ever try to add a node that would violate that. *)
    if debug then assert (height >= t.height_lower_bound);
    (* Whenever we set a node's height, we use [set_height], which enforces this. *)
    if debug then assert (height <= max_height_allowed t);
    node.height_in_adjust_heights_heap <- height;
    t.length <- t.length + 1;
    node.next_in_adjust_heights_heap <- t.nodes_by_height.(height);
    Array.unsafe_set t.nodes_by_height height (Uopt.some (Node.Packed.T node)))
;;

let remove_min_exn t : Node.Packed.t =
  if debug && is_empty t
  then
    failwiths ~here:[%here] "Adjust_heights_heap.remove_min of empty heap" t [%sexp_of: t];
  let r = ref t.height_lower_bound in
  while Uopt.is_none t.nodes_by_height.(!r) do
    incr r
  done;
  let height = !r in
  t.height_lower_bound <- height;
  let (T node) = Uopt.unsafe_value (Array.unsafe_get t.nodes_by_height height) in
  node.height_in_adjust_heights_heap <- -1;
  t.length <- t.length - 1;
  Array.unsafe_set t.nodes_by_height height node.next_in_adjust_heights_heap;
  node.next_in_adjust_heights_heap <- Uopt.none;
  T node
;;

let set_height t (node : _ Node.t) height =
  if height > t.max_height_seen
  then (
    t.max_height_seen <- height;
    if height > max_height_allowed t
    then
      failwiths
        ~here:[%here]
        "node with too large height"
        (`Height height, `Max (max_height_allowed t))
        [%sexp_of: [ `Height of int ] * [ `Max of int ]]);
  node.height <- height
;;

let ensure_height_requirement t ~original_child ~original_parent ~child ~parent =
  if debug then assert (Node.is_necessary child);
  if debug then assert (Node.is_necessary parent);
  if Node.same parent original_child
  then
    failwiths
      ~here:[%here]
      "adding edge made graph cyclic"
      (`child original_child, `parent original_parent)
      [%sexp_of: [ `child of _ Node.t ] * [ `parent of _ Node.t ]];
  if child.height >= parent.height
  then (
    add_unless_mem t parent;
    (* We set [parent.height] after adding [parent] to the heap, so that [parent] goes
       in the heap with its pre-adjusted height. *)
    set_height t parent (child.height + 1))
;;

let adjust_heights
      (type a b)
      t
      recompute_heap
      ~child:(original_child : a Node.t)
      ~parent:(original_parent : b Node.t)
  =
  if debug then assert (is_empty t);
  if debug then assert (original_child.height >= original_parent.height);
  t.height_lower_bound <- original_parent.height;
  ensure_height_requirement
    t
    ~original_child
    ~original_parent
    ~child:original_child
    ~parent:original_parent;
  while length t > 0 do
    let (T child) = remove_min_exn t in
    if Node.is_in_recompute_heap child
    then Recompute_heap.increase_height recompute_heap child;
    if child.num_parents > 0
    then (
      let (T parent) = Uopt.value_exn child.parent0 in
      ensure_height_requirement t ~original_child ~original_parent ~child ~parent;
      for parent_index = 1 to child.num_parents - 1 do
        let (T parent) = Uopt.value_exn child.parent1_and_beyond.(parent_index - 1) in
        ensure_height_requirement t ~original_child ~original_parent ~child ~parent
      done);
    match child.kind with
    | Bind_lhs_change { all_nodes_created_on_rhs; _ } ->
      let r = ref all_nodes_created_on_rhs in
      while Uopt.is_some !r do
        let (T node_on_rhs) = Uopt.unsafe_value !r in
        r := node_on_rhs.next_node_in_same_scope;
        if Node.is_necessary node_on_rhs
        then
          ensure_height_requirement
            t
            ~original_child
            ~original_parent
            ~child
            ~parent:node_on_rhs
      done
    | _ -> ()
  done;
  if debug then assert (is_empty t);
  if debug then assert (original_child.height < original_parent.height)
;;
