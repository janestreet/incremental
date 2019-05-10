open! Core_kernel
open! Import
module Node = Types.Node

type 'a edge = 'a Types.Expert.edge =
  { child : 'a Node.t
  ; on_change : 'a -> unit
  ; (* [index] is defined whenever the [edge] is in the [children] of some [t]. Then it is
       the index of this [edge] in that [children] array. It might seem redundant with all
       the other indexes we have, but it is necessary to remove children.  The index may
       change as sibling children are removed. *)
    mutable index : int Uopt.t
  }
[@@deriving sexp_of]

type packed_edge = Types.Expert.packed_edge = E : 'a edge -> packed_edge
[@@unboxed] [@@deriving sexp_of]

type 'a t = 'a Types.Expert.t =
  { f : unit -> 'a
  ; on_observability_change : is_now_observable:bool -> unit
  ; mutable children : packed_edge Uopt.t Array.t
  ; mutable num_children : int
  ; (* When set, makes the node of [t] stale.  It is set when the set of children changes.
       Otherwise the normal check of staleness (comparing the [changed_at] field of
       children and the [recomputed_at] field for the node of [t]) would not be enough.
       This plays a role similar to the cutoff of [Never] for the lhs-change of binds, but
       we don't have a special child. *)
    mutable force_stale : bool
  ; (* The number of invalid children that point to us.  Used to determine whether the node
       of [t] needs to invalidated, without iterating over all the children.  This is not
       needed for other nodes, because there are no other nodes that have a potentially
       large and dynamic set of children. *)
    mutable num_invalid_children : int
  ; (* Whether we will fire the [on_change] callbacks for all children when the node of [t]
       itself runs.  Used to make sure we rerun everything after [t] switches from
       unobservable and back to observable. *)
    mutable will_fire_all_callbacks : bool
  }
[@@deriving sexp_of]

let invariant
      _invariant_a
      { f = _
      ; children
      ; num_children
      ; force_stale = _
      ; num_invalid_children
      ; on_observability_change = _
      ; will_fire_all_callbacks = _
      }
  =
  assert (num_children <= Array.length children);
  ignore num_invalid_children;
  (* invariant is below, because we need some context *)
  Array.iteri children ~f:(fun i uopt ->
    match i < num_children with
    | true ->
      let (E r) = Uopt.value_exn uopt in
      [%test_result: int] (Uopt.value_exn r.index) ~expect:i
    | false -> assert (Uopt.is_none uopt))
;;

let invariant_about_num_invalid_children
      { children; num_children; num_invalid_children; _ }
      ~is_necessary
  =
  if not is_necessary
  then [%test_result: int] num_invalid_children ~expect:0
  else (
    let count_invalid_children = ref 0 in
    for i = 0 to num_children - 1 do
      let (E r) = Uopt.value_exn children.(i) in
      if not (Node.is_valid r.child) then incr count_invalid_children
    done;
    [%test_result: int] num_invalid_children ~expect:!count_invalid_children)
;;

let create ~f ~on_observability_change =
  { f
  ; on_observability_change
  ; children = [||]
  ; num_children = 0
  ; force_stale = false
  ; num_invalid_children = 0
  ; will_fire_all_callbacks = true
  }
;;

let make_stale t =
  if t.force_stale
  then `Already_stale
  else (
    t.force_stale <- true;
    `Ok)
;;

let incr_invalid_children t = t.num_invalid_children <- t.num_invalid_children + 1
let decr_invalid_children t = t.num_invalid_children <- t.num_invalid_children - 1

let make_space_for_child_if_necessary t =
  if t.num_children >= Array.length t.children
  then (
    if debug then assert (t.num_children = Array.length t.children);
    let new_max = Int.max 2 (2 * Array.length t.children) in
    t.children <- Array.realloc t.children ~len:new_max Uopt.none)
;;

let add_child_edge t packed_edge =
  let (E edge) = packed_edge in
  assert (Uopt.is_none edge.index);
  make_space_for_child_if_necessary t;
  let new_child_index = t.num_children in
  edge.index <- Uopt.some new_child_index;
  t.children.(new_child_index) <- Uopt.some packed_edge;
  t.num_children <- t.num_children + 1;
  t.force_stale <- true;
  (* We will bump the number of invalid children if necessary when connecting child and
     parent.  Same thing for running the [on_change] callbacks. *)
  new_child_index
;;

let swap_children t ~child_index1 ~child_index2 =
  let (E edge1) = Uopt.value_exn t.children.(child_index1) in
  let (E edge2) = Uopt.value_exn t.children.(child_index2) in
  edge1.index <- Uopt.some child_index2;
  edge2.index <- Uopt.some child_index1;
  Array.swap t.children child_index1 child_index2
;;

let last_child_edge_exn t =
  let last_index = t.num_children - 1 in
  Uopt.value_exn t.children.(last_index)
;;

let remove_last_child_edge_exn t =
  let last_index = t.num_children - 1 in
  let packed_edge_opt = t.children.(last_index) in
  t.children.(last_index) <- Uopt.none;
  t.num_children <- last_index;
  t.force_stale <- true;
  assert (Uopt.is_some packed_edge_opt);
  let (E edge) = Uopt.unsafe_value packed_edge_opt in
  edge.index <- Uopt.none
;;

let before_main_computation t =
  if t.num_invalid_children > 0
  then `Invalid
  else (
    t.force_stale <- false;
    let will_fire_all_callbacks = t.will_fire_all_callbacks in
    t.will_fire_all_callbacks <- false;
    if will_fire_all_callbacks
    then
      for i = 0 to t.num_children - 1 do
        let (E r) = Uopt.value_exn t.children.(i) in
        r.on_change (Uopt.value_exn r.child.value_opt)
      done;
    `Ok)
;;

let observability_change t ~is_now_observable =
  t.on_observability_change ~is_now_observable;
  if not is_now_observable
  then (
    t.will_fire_all_callbacks <- true;
    (* If we don't reset num_invalid_children, we would double count them: just imagine
       what happens we if reconnect/disconnect/reconnect/disconnect with an invalid
       child. *)
    t.num_invalid_children <- 0)
;;

let run_edge_callback t ~child_index =
  if not t.will_fire_all_callbacks
  then (
    let (E r) = Uopt.value_exn t.children.(child_index) in
    (* This value is not necessarily set, because we try to run this when connecting the
       node to its children, which could be before they have run even once.  Also the node
       could be invalid. *)
    if Uopt.is_some r.child.value_opt
    then r.on_change (Uopt.unsafe_value r.child.value_opt))
;;
