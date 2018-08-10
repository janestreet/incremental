open Core_kernel
open Import

module Node = Types.Node
module Packed_node = Types.Packed_node

type 'a t = 'a Types.Kind.t =
  | Array_fold              : (_, 'a) Array_fold.t -> 'a t
  | At                      : At.t -> Before_or_after.t t
  | At_intervals            : At_intervals.t -> unit t
  | Bind_lhs_change         : (_, _) Bind.t -> unit t
  | Bind_main               : (_, 'a) Bind.t -> 'a t
  | Const                  of 'a
  | Expert                 of 'a Expert.t
  | Freeze                 of 'a Freeze.t
  | If_test_change          : _ If_then_else.t -> unit t
  | If_then_else           of 'a If_then_else.t
  | Invalid
  | Join_lhs_change         : _ Join.t -> unit t
  | Join_main              of 'a Join.t
  | Map                     : ('a1 -> 'a) * 'a1 Node.t -> 'a t
  | Snapshot               of 'a Snapshot.t
  | Step_function          of 'a Step_function.t
  | Uninitialized
  | Unordered_array_fold    : (_, 'a) Unordered_array_fold.t -> 'a t
  | Var                    of 'a Var.t
  | Map2
    : ('a1 -> 'a2 -> 'a)
      * 'a1 Node.t * 'a2 Node.t
    -> 'a t
  | Map3
    : ('a1 -> 'a2 -> 'a3 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t
    -> 'a t
  | Map4
    : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t * 'a4 Node.t
    -> 'a t
  | Map5
    : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t * 'a4 Node.t * 'a5 Node.t
    -> 'a t
  | Map6
    : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t * 'a4 Node.t * 'a5 Node.t * 'a6 Node.t
    -> 'a t
  | Map7
    : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t * 'a4 Node.t * 'a5 Node.t * 'a6 Node.t
      * 'a7 Node.t
    -> 'a t
  | Map8
    : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t * 'a4 Node.t * 'a5 Node.t * 'a6 Node.t
      * 'a7 Node.t * 'a8 Node.t
    -> 'a t
  | Map9
    : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a)
      * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t * 'a4 Node.t * 'a5 Node.t * 'a6 Node.t
      * 'a7 Node.t * 'a8 Node.t * 'a9 Node.t
    -> 'a t
[@@deriving sexp_of]

let name : type a. a t       -> string = function
  | Array_fold _             -> "Array_fold"
  | At _                     -> "At"
  | At_intervals _           -> "At_intervals"
  | Bind_lhs_change _        -> "Bind_lhs_change"
  | Bind_main _              -> "Bind_main"
  | Const _                  -> "Const"
  | Expert _                 -> "Expert"
  | Freeze _                 -> "Freeze"
  | If_test_change _         -> "If_test_change"
  | If_then_else _           -> "If_then_else"
  | Invalid                  -> "Invalid"
  | Join_lhs_change _        -> "Join_lhs_change"
  | Join_main _              -> "Join_main"
  | Map _                    -> "Map"
  | Map2 _                   -> "Map2"
  | Map3 _                   -> "Map3"
  | Map4 _                   -> "Map4"
  | Map5 _                   -> "Map5"
  | Map6 _                   -> "Map6"
  | Map7 _                   -> "Map7"
  | Map8 _                   -> "Map8"
  | Map9 _                   -> "Map9"
  | Snapshot _               -> "Snapshot"
  | Step_function _          -> "Step_function"
  | Uninitialized            -> "Uninitialized"
  | Unordered_array_fold _   -> "Unordered_array_fold"
  | Var _                    -> "Var"
;;

let invariant : type a . a Invariant.t -> a t Invariant.t = fun invariant_a t ->
  match t with
  | Array_fold array_fold -> Array_fold.invariant ignore invariant_a array_fold
  | At at -> At.invariant at
  | At_intervals at_intervals -> At_intervals.invariant at_intervals
  | Bind_lhs_change bind -> Bind.invariant ignore ignore      bind
  | Bind_main       bind -> Bind.invariant ignore invariant_a bind
  | Const a -> invariant_a a
  | Expert e -> Expert.invariant invariant_a e
  | Freeze freeze -> Freeze.invariant invariant_a freeze
  | If_test_change if_then_else -> If_then_else.invariant ignore      if_then_else
  | If_then_else   if_then_else -> If_then_else.invariant invariant_a if_then_else
  | Invalid -> ()
  | Join_lhs_change join -> Join.invariant ignore      join
  | Join_main       join -> Join.invariant invariant_a join
  | Map  _ -> ()
  | Map2 _ -> ()
  | Map3 _ -> ()
  | Map4 _ -> ()
  | Map5 _ -> ()
  | Map6 _ -> ()
  | Map7 _ -> ()
  | Map8 _ -> ()
  | Map9 _ -> ()
  | Snapshot snapshot -> Snapshot.invariant invariant_a snapshot
  | Step_function step_function -> Step_function.invariant invariant_a step_function
  | Uninitialized -> ()
  | Unordered_array_fold unordered_array_fold ->
    Unordered_array_fold.invariant ignore invariant_a unordered_array_fold
  | Var var -> Var.invariant ignore var
;;

let initial_num_children (type a) (t : a t) =
  match t with
  | At _              -> 0
  | At_intervals _    -> 0
  | Bind_lhs_change _ -> 1
  | Bind_main _       -> 2
  | Const _           -> 0
  | Expert _          -> 0
  | Freeze _          -> 1
  | If_test_change _  -> 1
  | If_then_else _    -> 2
  | Invalid           -> 0
  | Join_lhs_change _ -> 1
  | Join_main _       -> 2
  | Map _             -> 1
  | Map2 _            -> 2
  | Map3 _            -> 3
  | Map4 _            -> 4
  | Map5 _            -> 5
  | Map6 _            -> 6
  | Map7 _            -> 7
  | Map8 _            -> 8
  | Map9 _            -> 9
  | Snapshot _        -> 0
  | Step_function _   -> 0
  | Uninitialized     -> 0
  | Var _             -> 0
  | Array_fold           { children; _ } -> Array.length children
  | Unordered_array_fold { children; _ } -> Array.length children
;;

let bind_rhs_child_index  = 1
let freeze_child_index    = 0
let if_branch_child_index = 1
let join_rhs_child_index  = 1

(* We do not implement the time-based nodes ([At], [At_intervals], [Snapshot],
   [Step_function]) as parents of the current-time node for performance reasons.  We don't
   want all such nodes to be recomputed whenever the time changes, which would be horribly
   inneficient.  Instead, we only want them to be recomputed at the "right" time,
   i.e. when time passes some threshold relevant to them.  We do this via scheduling
   alarms at those thresholds. *)
let iteri_children (type a) (t : a t) ~(f : int -> Packed_node.t -> unit) : unit =
  match t with
  | Array_fold { children; _ } ->
    for i = 0 to Array.length children - 1 do
      f i (Node.pack (Array.unsafe_get children i));
    done;
  | At _ -> ()
  | At_intervals _ -> ()
  | Bind_lhs_change bind -> f 0 (Node.pack bind.lhs)
  | Bind_main { lhs_change; rhs; _ } ->
    (* Various code, e.g. [state.became_necessary], relies on processing [lhs_change]
       before [rhs]. *)
    f 0 (Node.pack lhs_change);
    if Uopt.is_some rhs then f 1 (Node.pack (Uopt.unsafe_value rhs));
  | Const _ -> ()
  | Expert { children; num_children; _ } ->
    for i = 0 to num_children - 1; do
      let Expert.E r = Uopt.value_exn (Array.unsafe_get children i)  in
      f i (Node.pack r.child)
    done
  | Freeze { child; _ } -> f 0 (Node.pack child)
  | If_test_change { test; _ } -> f 0 (Node.pack test)
  | If_then_else { test_change; current_branch; _ } ->
    f 0 (Node.pack test_change);
    if Uopt.is_some current_branch then f 1 (Node.pack (Uopt.unsafe_value current_branch));
  | Invalid -> ()
  | Join_lhs_change { lhs; _ } -> f 0 (Node.pack lhs)
  | Join_main { lhs_change; rhs; _ }  ->
    f 0 (Node.pack lhs_change);
    if Uopt.is_some rhs then f 1 (Node.pack (Uopt.unsafe_value rhs));
  | Snapshot _ -> ()
  | Step_function _ -> ()
  | Uninitialized -> ()
  | Unordered_array_fold { children; _ } ->
    for i = 0 to Array.length children - 1 do
      f i (Node.pack (Array.unsafe_get children i));
    done;
  | Var _ -> ()
  | Map (_, node0) ->
    f 0 (Node.pack node0)
  | Map2 (_, node0, node1) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
  | Map3 (_, node0, node1, node2) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
  | Map4 (_, node0, node1, node2, node3) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
    f 3 (Node.pack node3);
  | Map5 (_, node0, node1, node2, node3, node4) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
    f 3 (Node.pack node3);
    f 4 (Node.pack node4);
  | Map6 (_, node0, node1, node2, node3, node4, node5) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
    f 3 (Node.pack node3);
    f 4 (Node.pack node4);
    f 5 (Node.pack node5);
  | Map7 (_, node0, node1, node2, node3, node4, node5, node6) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
    f 3 (Node.pack node3);
    f 4 (Node.pack node4);
    f 5 (Node.pack node5);
    f 6 (Node.pack node6);
  | Map8 (_, node0, node1, node2, node3, node4, node5, node6, node7) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
    f 3 (Node.pack node3);
    f 4 (Node.pack node4);
    f 5 (Node.pack node5);
    f 6 (Node.pack node6);
    f 7 (Node.pack node7);
  | Map9 (_, node0, node1, node2, node3, node4, node5, node6, node7, node8) ->
    f 0 (Node.pack node0);
    f 1 (Node.pack node1);
    f 2 (Node.pack node2);
    f 3 (Node.pack node3);
    f 4 (Node.pack node4);
    f 5 (Node.pack node5);
    f 6 (Node.pack node6);
    f 7 (Node.pack node7);
    f 8 (Node.pack node8);
;;

(* [slow_get_child] is only used by [Node.invariant], so we don't mind using [with_return]
   and [iteri_children].  If we ever need a fast [get_child], we coded it in rev
   48dbfd03c9c5. *)
let slow_get_child : type a . a t -> index:_ -> _ =
  fun t ~index ->
    match t with
    | Array_fold { children; _ }           -> Node.pack children.( index )
    | Unordered_array_fold { children; _ } -> Node.pack children.( index )
    | Expert { children; _ } ->
      let Expert.E edge = Uopt.value_exn children.( index ) in
      Node.pack edge.child
    | _ ->
      with_return (fun r ->
        iteri_children t ~f:(fun i child -> if i = index then r.return child);
        failwiths "Kind.slow_get_child got invalid index" (index, t)
          [%sexp_of: int * _ t]);
;;
