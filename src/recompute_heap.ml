open Core_kernel
open Import

module As_recompute_list = Node.Packed.As_list (struct
    let next (Node.Packed.T node) = node.next_in_recompute_heap
  end)

module Nodes_by_height = struct
  type t = As_recompute_list.t Array.t [@@deriving sexp_of]

  (* We display the smallest prefix of [nodes_by_height] that includes all nodes. *)
  let sexp_of_t t =
    let max_nonempty_index = ref (-1) in
    Array.iteri t ~f:(fun i l -> if Uopt.is_some l then max_nonempty_index := i);
    Array.sub t ~pos:0 ~len:(!max_nonempty_index + 1) |> [%sexp_of: t]
  ;;
end

type t = Types.Recompute_heap.t =
  { mutable length : int
  ; mutable height_lower_bound : int
  ; mutable nodes_by_height : Nodes_by_height.t
  }
[@@deriving fields, sexp_of]

let max_height_allowed t = Array.length t.nodes_by_height - 1
let is_empty t = t.length = 0

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~length:
        (check (fun length ->
           let actual_length = ref 0 in
           Array.iter t.nodes_by_height ~f:(fun node ->
             actual_length := !actual_length + As_recompute_list.length node);
           [%test_eq: int] length !actual_length))
      ~height_lower_bound:
        (check (fun height_lower_bound ->
           assert (height_lower_bound >= 0);
           assert (height_lower_bound <= Array.length t.nodes_by_height);
           for height = 0 to height_lower_bound - 1 do
             assert (Uopt.is_none t.nodes_by_height.(height))
           done))
      ~nodes_by_height:
        (check (fun nodes_by_height ->
           Array.iteri nodes_by_height ~f:(fun height node ->
             As_recompute_list.iter node ~f:(fun (T node) ->
               assert (node.height_in_recompute_heap = height);
               assert (Node.needs_to_be_computed node))))))
;;

let create_nodes_by_height ~max_height_allowed =
  Array.create ~len:(max_height_allowed + 1) Uopt.none
;;

let set_max_height_allowed t max_height_allowed =
  if debug
  then
    for i = max_height_allowed + 1 to Array.length t.nodes_by_height - 1 do
      assert (Uopt.is_none t.nodes_by_height.(i))
    done;
  let src = t.nodes_by_height in
  let dst = create_nodes_by_height ~max_height_allowed in
  Array.blit
    ~src
    ~src_pos:0
    ~dst
    ~dst_pos:0
    ~len:(min (Array.length src) (Array.length dst));
  t.nodes_by_height <- dst;
  t.height_lower_bound <- min t.height_lower_bound (Array.length dst)
;;

let create ~max_height_allowed =
  { length = 0
  ; height_lower_bound = max_height_allowed + 1
  ; nodes_by_height = create_nodes_by_height ~max_height_allowed
  }
;;

let set_next (prev : Node.Packed.t Uopt.t) ~next =
  if Uopt.is_some prev
  then (
    let (T prev) = Uopt.unsafe_value prev in
    prev.next_in_recompute_heap <- next)
;;

let set_prev (next : Node.Packed.t Uopt.t) ~prev =
  if Uopt.is_some next
  then (
    let (T next) = Uopt.unsafe_value next in
    next.prev_in_recompute_heap <- prev)
;;

let link (type a) t (node : a Node.t) =
  let height = node.height in
  if debug then assert (height <= max_height_allowed t);
  node.height_in_recompute_heap <- height;
  let next = t.nodes_by_height.(height) in
  node.next_in_recompute_heap <- next;
  set_prev next ~prev:(Uopt.some (Node.Packed.T node));
  Array.unsafe_set t.nodes_by_height height (Uopt.some (Node.Packed.T node))
;;

let unlink (type a) t (node : a Node.t) =
  let prev = node.prev_in_recompute_heap in
  let next = node.next_in_recompute_heap in
  if phys_same (Uopt.some node) t.nodes_by_height.(node.height_in_recompute_heap)
  then Array.unsafe_set t.nodes_by_height node.height_in_recompute_heap next;
  set_prev next ~prev;
  set_next prev ~next;
  node.prev_in_recompute_heap <- Uopt.none
;;

(* We don't set [node.next_in_recompute_heap] here, but rather after calling [unlink]. *)

let add (type a) t (node : a Node.t) =
  if debug && (Node.is_in_recompute_heap node || not (Node.needs_to_be_computed node))
  then
    failwiths
      ~here:[%here]
      "incorrect attempt to add node to recompute heap"
      node
      [%sexp_of: _ Node.t];
  if debug then assert (node.height <= max_height_allowed t);
  let height = node.height in
  if height < t.height_lower_bound then t.height_lower_bound <- height;
  link t node;
  t.length <- t.length + 1
;;

let remove (type a) t (node : a Node.t) =
  if debug && ((not (Node.is_in_recompute_heap node)) || Node.needs_to_be_computed node)
  then
    failwiths
      ~here:[%here]
      "incorrect [remove] of node from recompute heap"
      node
      [%sexp_of: _ Node.t];
  unlink t node;
  node.next_in_recompute_heap <- Uopt.none;
  node.height_in_recompute_heap <- -1;
  t.length <- t.length - 1
;;

let increase_height (type a) t (node : a Node.t) =
  if debug
  then (
    assert (node.height > node.height_in_recompute_heap);
    assert (node.height <= max_height_allowed t);
    assert (Node.is_in_recompute_heap node));
  unlink t node;
  link t node
;;

let min_height t =
  if t.length = 0
  then t.height_lower_bound <- Array.length t.nodes_by_height
  else (
    let nodes_by_height = t.nodes_by_height in
    while Uopt.is_none nodes_by_height.(t.height_lower_bound) do
      t.height_lower_bound <- t.height_lower_bound + 1
    done);
  t.height_lower_bound
;;

let remove_min t : Node.Packed.t =
  if debug then assert (not (is_empty t));
  let nodes_by_height = t.nodes_by_height in
  let node = ref nodes_by_height.(t.height_lower_bound) in
  while Uopt.is_none !node do
    t.height_lower_bound <- t.height_lower_bound + 1;
    if debug && t.height_lower_bound >= Array.length t.nodes_by_height
    then
      failwiths
        ~here:[%here]
        "Recompute_heap.remove_min unexpectedly reached end of heap"
        t
        [%sexp_of: t];
    node := nodes_by_height.(t.height_lower_bound)
  done;
  let (T node) = Uopt.unsafe_value !node in
  node.height_in_recompute_heap <- -1;
  t.length <- t.length - 1;
  let next = node.next_in_recompute_heap in
  t.nodes_by_height.(t.height_lower_bound) <- next;
  set_prev next ~prev:Uopt.none;
  if debug then assert (Uopt.is_none node.prev_in_recompute_heap);
  node.next_in_recompute_heap <- Uopt.none;
  T node
;;
