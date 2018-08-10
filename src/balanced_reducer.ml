open! Core_kernel
open! Import

(* The [data] array is an implicit binary tree with [children_length * 2 - 1] nodes,
   with each node being the sum of the two child nodes and the root node being the 0th
   node.  The leaves of the tree are the last [num_leaves] nodes.

   The children are not necessarily all at the same level of the tree. For instance if
   you have 3 children [| a; b; c |]:

   {v
          o
         / \
        o   c
       / \
      a   b
   v}

   We want this tree to be representated as [| o; o; c; a; b |], i.e. we need to apply
   first a rotation then a translation to convert an index in [| a; b; c |] to a (leaf)
   index in [| o; o; c; a; b |]. *)
type 'a t =
  { data                           : 'a Option_array.t
  ; num_leaves                     : int
  ; num_leaves_not_in_bottom_level : int
  ; reduce                         : 'a -> 'a -> 'a
  ; sexp_of_a                      : ('a -> Sexp.t)
  }

let length t = t.num_leaves

(* {v
     parent:      0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 ...
     left child:  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 ...
     right child: 2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 ... v} *)
let parent_index      ~child_index      = (child_index - 1) / 2
let left_child_index  ~parent_index     = parent_index * 2 + 1
let right_child_index ~left_child_index = left_child_index + 1

(* The first [num_leaves-1] elements are internal nodes of the tree.  The next
   [num_leaves] elements are the leaves. *)
let num_branches    t   = t.num_leaves - 1
let index_is_leaf   t i = i >= num_branches t

(* The tree is complete, but not necessarily perfect, so we perform some rotation of the
   leaves to ensure that our reductions preserve ordering. *)
let leaf_index t i =
  (* The tree layout is level order.  Any leaves in the second to last level need to occur
     in the array before the leaves in the bottom level. *)
  let rotated_index =
    let offset_from_start_of_leaves_in_array = i + t.num_leaves_not_in_bottom_level in
    if offset_from_start_of_leaves_in_array < t.num_leaves
    then offset_from_start_of_leaves_in_array
    else offset_from_start_of_leaves_in_array - t.num_leaves
  in
  (* The leaves occur after the branches in the array. *)
  rotated_index + num_branches t
;;

let get_leaf t i = Option_array.get t.data (leaf_index t i)

let to_list t = List.init (length t) ~f:(fun i -> get_leaf t i)

let sexp_of_t sexp_of_a t = [%sexp (to_list t : a option list)]

let invariant invariant_a t =
  let data = t.data in
  for i = 0 to Option_array.length data - 1 do
    match Option_array.get data i with
    | None -> ()
    | Some a -> invariant_a a
  done;
  for i = 0 to num_branches t - 1 do
    let left  = left_child_index  ~parent_index:i        in
    let right = right_child_index ~left_child_index:left in
    let left_is_none  = Option_array.is_none data left  in
    let right_is_none = Option_array.is_none data right in
    if Option_array.is_some data i
    then assert (not (left_is_none || right_is_none))
    else assert (index_is_leaf t left
                 || index_is_leaf t right
                 || left_is_none
                 || right_is_none);
  done
;;

let create_exn ?(sexp_of_a = [%sexp_of: _]) () ~len:num_leaves ~reduce =
  if num_leaves < 1
  then raise_s [%message
         "non-positive number of leaves in balanced reducer" (num_leaves : int)];
  let num_branches = num_leaves - 1 in
  let num_leaves_not_in_bottom_level = Int.ceil_pow2 num_leaves - num_leaves in
  let data = Option_array.create ~len:(num_branches + num_leaves) in
  { data
  ; num_leaves
  ; num_leaves_not_in_bottom_level
  ; reduce
  ; sexp_of_a }
;;

let set_exn t i a =
  if i < 0
  then raise_s [%message
         "attempt to set negative index in balanced reducer" ~index:(i : int)];
  let length = t.num_leaves in
  if i >= length
  then raise_s [%message
         "attempt to set out of bounds index in balanced reducer"
           ~index:(i : int)
           (length : int)];
  let data = t.data in
  let i = ref (leaf_index t i) in
  Option_array.set_some data !i a;
  while !i <> 0 do
    let parent = parent_index ~child_index:!i in
    if Option_array.is_none data parent
    then i := 0
    else (
      Option_array.unsafe_set_none data parent;
      i := parent);
  done;
;;

let rec compute_exn t i =
  if Option_array.is_some t.data i
  then Option_array.unsafe_get_some_exn t.data i
  else (
    let left  = left_child_index  ~parent_index:i        in
    let right = right_child_index ~left_child_index:left in
    if left >= Option_array.length t.data
    then (
      (* If we get here, the parent was an unset leaf. *)
      let sexp_of_a = t.sexp_of_a in
      raise_s [%message
        "attempt to compute balanced reducer with unset elements"
          ~balanced_reducer:(t : a t)]);
    let a = t.reduce (compute_exn t left) (compute_exn t right) in
    Option_array.unsafe_set_some t.data i a;
    a)
;;

let compute_exn t = compute_exn t 0
