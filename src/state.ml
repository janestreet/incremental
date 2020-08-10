(* [State] defines the global state of which there is one instance for each call to
   [Incremental.Make].

   This module does not have an mli because it would essentially duplicate
   [Incremental.S], except adding an extra [State.t] argument to functions. *)

open Core_kernel
open Import
open Types.Kind

type status = Types.Status.t =
  | Stabilizing
  | Running_on_update_handlers
  | Not_stabilizing
  | Stabilize_previously_raised of Raised_exn.t
[@@deriving sexp_of]

module Node_update = On_update_handler.Node_update

module Run_on_update_handlers = struct
  type t = Types.Run_on_update_handlers.t = T : 'a Node.t * 'a Node_update.t -> t
  [@@deriving sexp_of]

  let invariant (T (node, _node_update) as t) =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () -> Node.invariant ignore node)
  ;;
end

module Only_in_debug = struct
  (* Extra state kept only when [debug] for the purpose of writing assertions. *)
  type t = Types.Only_in_debug.t =
    { mutable currently_running_node : Node.Packed.t option
    ; mutable expert_nodes_created_by_current_node : Node.Packed.t list
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      Fields.iter
        ~currently_running_node:ignore
        ~expert_nodes_created_by_current_node:ignore)
  ;;

  let create () =
    { currently_running_node = None; expert_nodes_created_by_current_node = [] }
  ;;
end

module Packed_weak_hashtbl = struct
  type t = Types.Packed_weak_hashtbl.t = T : (_, _) Weak_hashtbl.t -> t
  [@@deriving sexp_of]
end

type t = Types.State.t =
  { mutable status : status
  ; bind_lhs_change_should_invalidate_rhs : bool
  ; (* [stabilization_num] starts at zero, and is incremented at the end of each
       stabilization. *)
    mutable stabilization_num : Stabilization_num.t
  ; mutable current_scope : Scope.t
  ; recompute_heap : Recompute_heap.t
  ; adjust_heights_heap : Adjust_heights_heap.t
  ; (* [propagate_invalidity] holds nodes that have invalid children that should be
       considered for invalidation.  It is only used during graph restructuring:
       [invalidate_node] and [add_parent].  Once an element is added to the stack, we then
       iterate until invalidity has propagated to all ancestors as necessary, according to
       [Node.should_be_invalidated]. *)
    propagate_invalidity : Node.Packed.t Stack.t
  ; (* [num_active_observers] is the number of observers whose state is [Created] or
       [In_use]. *)
    mutable num_active_observers : int
  ; (* [all_observers] is the doubly-linked list of all observers in effect, or that have
       been disallowed since the most recent start of a stabilization -- these have
       [state] as [In_use] or [Disallowed]. *)
    mutable all_observers : Internal_observer.Packed.t Uopt.t
  ; (* We enqueue finalized observers in a thread-safe queue, for handling during
       stabilization.  We use a thread-safe queue because OCaml finalizers can run in any
       thread. *)
    finalized_observers : Internal_observer.Packed.t Thread_safe_queue.t
  ; (* [new_observers] holds observers created since the most recent start of a
       stabilization -- these have [state] as [Created] or [Unlinked].  At the start of
       stabilization, we link into [all_observers] all observers in [new_observers] whose
       state is [Created] and add them to the [observers] of the node they are observing.
       We structure things this way to allow observers to be created during stabilization
       while running user code ([map], [bind], etc), but to not have to deal with nodes
       becoming necessary and the the graph changing during such code. *)
    new_observers : Internal_observer.Packed.t Stack.t
  ; (* [disallowed_observers] holds all observers that have been disallowed since the most
       recent start of a stabilization -- these have [state = Disallowed].  At the start
       of stabilization, these are unlinked from [all_observers] and their state is
       changed to [Unlinked].  We structure things this way to allow user code running
       during stabilization to call [disallow_future_use], but to not have to deal with
       nodes becoming unnecessary and the graph changing during such code. *)
    disallowed_observers : Internal_observer.Packed.t Stack.t
  ; (* We delay all [Var.set] calls that happen during stabilization so that they take
       effect after stabilization.  All variables set during stabilization are pushed on
       [set_during_stabilization] rather than setting them.  Then, after the graph has
       stabilized, we do all the sets, so that they take effect at the start of the next
       stabilization. *)
    set_during_stabilization : Var.Packed.t Stack.t
  ; (* [handle_after_stabilization] has all nodes with handlers to consider running at the
       end of the next stabilization.  At the end of stabilization, we consider each node
       in [handle_after_stabilization], and if we decide to run its on-update handlers,
       push it on [run_on_update_handlers].  Then, once we've considered all nodes in
       [handle_after_stabilization], we iterate through [run_on_update_handlers] and
       actually run the handlers.

       These two passes are essential for correctness.  During the first pass, we haven't
       run any user handlers, so we know that the state is exactly as it was when
       stabilization finished.  In particular, we know that if a node is necessary, then
       it has a stable value; once user handlers run, we don't know this.  During the
       second pass, user handlers can make calls to any incremental function except for
       [stabilize].  In particular, some functions push nodes on
       [handle_after_stabilization].  But no functions (except for [stabilize]) modify
       [run_on_update_handlers]. *)
    handle_after_stabilization : Node.Packed.t Stack.t
  ; run_on_update_handlers : Run_on_update_handlers.t Stack.t
  ; mutable only_in_debug : Only_in_debug.t
  ; weak_hashtbls : Packed_weak_hashtbl.t Thread_safe_queue.t
  ; mutable keep_node_creation_backtrace : bool
  ; (* Stats.  These are all incremented at the appropriate place, and never decremented. *)
    mutable num_nodes_became_necessary : int
  ; mutable num_nodes_became_unnecessary : int
  ; mutable num_nodes_changed : int
  ; mutable num_nodes_created : int
  ; mutable num_nodes_invalidated : int
  ; mutable num_nodes_recomputed : int
  ; mutable num_nodes_recomputed_directly_because_one_child : int
  ; mutable num_nodes_recomputed_directly_because_min_height : int
  ; mutable num_var_sets : int
  }
[@@deriving fields, sexp_of]

module Clock = struct
  type t = Types.Clock.t =
    { (* We use [timing_wheel] for time-based incrementals.  [now] is a variable holding
         the current time.  [handle_fired] is the closure passed to
         [Timing_wheel.advance_clock].  It links all the fired alarm values into
         [fired_alarm_values].  After [Timing_wheel.advance_clock] returns, it then
         walks through the linked list and actually fires them.  This two-pass approach is
         necessary because one is not allowed to call [Timing_wheel] functions from the
         [handle_fired] that one passes to [Timing_wheel.advance_clock]. *)
      timing_wheel : Alarm_value.t Timing_wheel.t
    ; now : Time_ns.t Var.t
    ; handle_fired : Alarm.t -> unit
    ; mutable fired_alarm_values : Alarm_value.t Uopt.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~now:
          (check (fun (now : _ Var.t) ->
             assert (Time_ns.equal now.value (Timing_wheel.now t.timing_wheel))))
        ~handle_fired:ignore
        ~fired_alarm_values:
          (check (fun fired_alarm_values -> assert (Uopt.is_none fired_alarm_values)))
        ~timing_wheel:(check (Timing_wheel.invariant Alarm_value.invariant)))
  ;;

  let incr_state t = Var.incr_state t.now
end

let now (clock : Clock.t) = clock.now.value
let timing_wheel_length (clock : Clock.t) = Timing_wheel.length clock.timing_wheel
let num_stabilizes t = Stabilization_num.to_int t.stabilization_num
let max_height_allowed t = Adjust_heights_heap.max_height_allowed t.adjust_heights_heap
let max_height_seen t = Adjust_heights_heap.max_height_seen t.adjust_heights_heap

let iter_observers t ~f =
  let r = ref t.all_observers in
  while Uopt.is_some !r do
    let observer = Uopt.unsafe_value !r in
    r := Internal_observer.Packed.next_in_all observer;
    f observer
  done
;;

let directly_observed t =
  let r : Node.Packed.t list ref = ref [] in
  iter_observers t ~f:(fun (T internal_observer) ->
    r := T internal_observer.observing :: !r);
  !r
;;

let save_dot t file = Node.Packed.save_dot file (directly_observed t)
let iter_observer_descendants t ~f = Node.Packed.iter_descendants (directly_observed t) ~f

module Stats = struct
  type t =
    { max_num_parents : int
    ; percentage_of_nodes_by_num_parents : (int * Percent.t) list
    }
  [@@deriving sexp]
end

let stats t =
  let max_num_parents = ref (-1) in
  let num_necessary_nodes = ref 0 in
  iter_observer_descendants t ~f:(fun (T node) ->
    incr num_necessary_nodes;
    max_num_parents := Int.max !max_num_parents node.num_parents);
  let max_num_parents = !max_num_parents in
  let num_nodes_by_num_parents = Array.create ~len:(max_num_parents + 1) 0 in
  iter_observer_descendants t ~f:(fun (T node) ->
    let num_parents = node.num_parents in
    num_nodes_by_num_parents.(num_parents) <- num_nodes_by_num_parents.(num_parents) + 1);
  let percentage_of_nodes_by_num_parents =
    Array.foldi num_nodes_by_num_parents ~init:[] ~f:(fun i ac num_nodes ->
      if num_nodes = 0
      then ac
      else (i, Percent.of_mult (float num_nodes /. float !num_necessary_nodes)) :: ac)
    |> List.rev
  in
  { Stats.max_num_parents; percentage_of_nodes_by_num_parents }
;;

let am_stabilizing t =
  match t.status with
  | Running_on_update_handlers | Stabilizing -> true
  | Not_stabilizing -> false
  | Stabilize_previously_raised raised_exn ->
    failwiths
      ~here:[%here]
      "cannot call am_stabilizing -- stabilize previously raised"
      raised_exn
      [%sexp_of: Raised_exn.t]
;;

let invariant t =
  match t.status with
  | Stabilize_previously_raised _ -> ()
  | Running_on_update_handlers | Stabilizing | Not_stabilizing ->
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      iter_observers t ~f:(fun (T internal_observer) ->
        (match internal_observer.state with
         | In_use | Disallowed -> ()
         | Created | Unlinked ->
           failwiths
             ~here:[%here]
             "member of all_observers with unexpected state"
             internal_observer
             [%sexp_of: _ Internal_observer.t]);
        Internal_observer.invariant ignore internal_observer);
      iter_observer_descendants t ~f:(fun (T node) ->
        Node.invariant ignore node;
        if not (am_stabilizing t) then assert (Uopt.is_none node.old_value_opt);
        assert (
          node.height <= Adjust_heights_heap.max_height_seen t.adjust_heights_heap));
      assert (
        Adjust_heights_heap.max_height_allowed t.adjust_heights_heap
        = Recompute_heap.max_height_allowed t.recompute_heap);
      Fields.iter
        ~status:ignore
        ~bind_lhs_change_should_invalidate_rhs:ignore
        ~stabilization_num:(check Stabilization_num.invariant)
        ~current_scope:
          (check (fun current_scope -> assert (phys_equal current_scope Scope.top)))
        ~recompute_heap:(check Recompute_heap.invariant)
        ~adjust_heights_heap:
          (check (fun adjust_heights_heap ->
             assert (Adjust_heights_heap.length adjust_heights_heap = 0);
             Adjust_heights_heap.invariant adjust_heights_heap))
        ~propagate_invalidity:
          (check (fun propagate_invalidity ->
             assert (Stack.is_empty propagate_invalidity)))
        ~num_active_observers:
          (check (fun num_active_observers -> assert (num_active_observers >= 0)))
        ~all_observers:ignore
        ~finalized_observers:ignore
        ~new_observers:
          (check
             (Stack.invariant (fun packed ->
                Internal_observer.Packed.invariant packed;
                let (T internal_observer) = packed in
                (* When an observer is added to [new_observers], it has [state = Created].
                   The only possible transitions from there are to [Unlinked] or to
                   [In_use], which also removes it from [new_observers], never to be added
                   again.  Thus it is impossible for an observer in [new_observers] to be
                   [In_use] or [Disallowed]. *)
                match internal_observer.state with
                | Created | Unlinked -> ()
                | In_use | Disallowed -> assert false)))
        ~disallowed_observers:
          (check
             (Stack.invariant (fun packed ->
                Internal_observer.Packed.invariant packed;
                let (T internal_observer) = packed in
                match internal_observer.state with
                | Disallowed -> ()
                | Created | In_use | Unlinked -> assert false)))
        ~set_during_stabilization:
          (check (fun set_during_stabilization ->
             match t.status with
             | Stabilize_previously_raised _ -> assert false
             | Running_on_update_handlers | Not_stabilizing ->
               assert (Stack.is_empty set_during_stabilization)
             | Stabilizing ->
               Stack.invariant
                 (fun (Var.Packed.T var) ->
                    assert (Uopt.is_some var.value_set_during_stabilization))
                 set_during_stabilization))
        ~handle_after_stabilization:(check (Stack.invariant Node.Packed.invariant))
        ~run_on_update_handlers:
          (check (Stack.invariant Run_on_update_handlers.invariant))
        ~only_in_debug:(check Only_in_debug.invariant)
        ~weak_hashtbls:ignore
        ~keep_node_creation_backtrace:ignore
        ~num_nodes_became_necessary:ignore
        ~num_nodes_became_unnecessary:ignore
        ~num_nodes_changed:ignore
        ~num_nodes_created:ignore
        ~num_nodes_invalidated:ignore
        ~num_nodes_recomputed:ignore
        ~num_nodes_recomputed_directly_because_one_child:ignore
        ~num_nodes_recomputed_directly_because_min_height:ignore
        ~num_var_sets:ignore)
;;

let ensure_not_stabilizing t ~name ~allow_in_update_handler =
  match t.status with
  | Not_stabilizing -> ()
  | Running_on_update_handlers ->
    if not allow_in_update_handler
    then (
      let backtrace = Backtrace.get () in
      failwiths
        ~here:[%here]
        (sprintf "cannot %s during on-update handlers" name)
        backtrace
        [%sexp_of: Backtrace.t])
  | Stabilize_previously_raised raised_exn ->
    let backtrace = Backtrace.get () in
    failwiths
      ~here:[%here]
      (sprintf "cannot %s -- stabilize previously raised" name)
      (raised_exn, backtrace)
      [%sexp_of: Raised_exn.t * Backtrace.t]
  | Stabilizing ->
    let backtrace = Backtrace.get () in
    failwiths
      ~here:[%here]
      (sprintf "cannot %s during stabilization" name)
      backtrace
      [%sexp_of: Backtrace.t]
;;

let set_height (node : _ Node.t) height =
  let t = node.state in
  Adjust_heights_heap.set_height t.adjust_heights_heap node height
;;

let set_max_height_allowed t height =
  ensure_not_stabilizing t ~name:"set_max_height_allowed" ~allow_in_update_handler:true;
  Adjust_heights_heap.set_max_height_allowed t.adjust_heights_heap height;
  Recompute_heap.set_max_height_allowed t.recompute_heap height
;;

let handle_after_stabilization : type a. a Node.t -> unit =
  fun node ->
  if not node.is_in_handle_after_stabilization
  then (
    let t = node.state in
    node.is_in_handle_after_stabilization <- true;
    Stack.push t.handle_after_stabilization (T node))
;;

let rec remove_children : type a. a Node.t -> unit =
  fun parent ->
  Node.iteri_children parent ~f:(fun child_index (T child) ->
    remove_child ~child ~parent ~child_index)

and remove_child : type a b. child:b Node.t -> parent:a Node.t -> child_index:int -> unit =
  fun ~child ~parent ~child_index ->
  Node.remove_parent ~child ~parent ~child_index;
  check_if_unnecessary child

and check_if_unnecessary : type a. a Node.t -> unit =
  fun node -> if not (Node.is_necessary node) then became_unnecessary node

and became_unnecessary : type a. a Node.t -> unit =
  fun node ->
  let t = node.state in
  t.num_nodes_became_unnecessary <- t.num_nodes_became_unnecessary + 1;
  if node.num_on_update_handlers > 0 then handle_after_stabilization node;
  node.height <- -1;
  remove_children node;
  (match node.kind with
   | Unordered_array_fold u -> Unordered_array_fold.force_full_compute u
   | Expert p -> Expert.observability_change p ~is_now_observable:false
   | _ -> ());
  if debug then assert (not (Node.needs_to_be_computed node));
  if Node.is_in_recompute_heap node then Recompute_heap.remove t.recompute_heap node
;;

let remove_alarm (clock : Clock.t) alarm =
  if Timing_wheel.mem clock.timing_wheel alarm
  then Timing_wheel.remove clock.timing_wheel alarm
;;

(* An invalid node is node whose kind is [Invalid].  A node's kind is set to [Invalid]
   when the lhs of its scope changes, or one if its children propagate the invalidity
   upward (see [Node.should_be_invalidated] to see in which case invalidity propagation
   stops).  Invalidating a node disconnects it from its children, which means:

   1. an invalid node cannot end up on the scheduler (if it is on the scheduler when
   it is invalidated, it is removed)
   2. an invalid node doesn't make its children necessary anymore.

   Invalid nodes usually have no parents, because the upward invalidity propagation means
   that their parents will themselves become invalid and disconnect from their children.
   However, [if], [join] or [bind] are not invalidated by the upward propagation, so an
   invalid node can still have parents.  Invalid nodes can be necessary, in the case where
   they have parents, and also when they are observed.

   The upward propagation of invalidity happens both when a node becomes invalid, and when
   trying to add an edge from an invalid child node to another node.  Because invalidity
   is only propagated upward, and because the rhs of a bind is invalidated before it
   executes, a node cannot be both computed and invalidated in the same stabilization.

   When invalidating, we can't assume much about the nodes we visit.  We cannot assume
   that nodes are valid (the rhs can contain previously invalidated nodes), or that nodes
   are unnecessary (nodes can be made necessary without going through their containing
   binds). *)

let rec invalidate_node : type a. a Node.t -> unit =
  fun node ->
  if Node.is_valid node
  then (
    let t = node.state in
    if node.num_on_update_handlers > 0 then handle_after_stabilization node;
    node.value_opt <- Uopt.none;
    if debug then assert (Uopt.is_none node.old_value_opt);
    node.changed_at <- t.stabilization_num;
    node.recomputed_at <- t.stabilization_num;
    t.num_nodes_invalidated <- t.num_nodes_invalidated + 1;
    if Node.is_necessary node
    then (
      remove_children node;
      (* The node doesn't have children anymore, so we can lower its height as much as
         possible, to one greater than the scope it was created in.  Also, because we
         are lowering the height, we don't need to adjust any of its ancestors' heights.
         We could leave the height alone, but we may as well lower it as much as
         possible to avoid making the heights of any future ancestors unnecessarily
         large. *)
      node.height <- Scope.height node.created_in + 1);
    (* We don't set [node.created_in] or [node.next_node_in_same_scope]; we leave [node]
       in the scope it was created in.  If that scope is ever invalidated, then that
       will clear [node.next_node_in_same_scope] *)
    (match node.kind with
     | At at -> remove_alarm at.clock at.alarm
     | At_intervals at_intervals -> remove_alarm at_intervals.clock at_intervals.alarm
     | Bind_main bind -> invalidate_nodes_created_on_rhs bind.all_nodes_created_on_rhs
     | Step_function { alarm; clock; _ } -> remove_alarm clock alarm
     | _ -> ());
    Node.set_kind node Invalid;
    (* If we called [propagate_invalidity] right away on the parents, we would get into
       trouble.  The parent would disconnect itself from the current node, thus
       modifying the list of parents we iterate on.  Even if we made a special case, it
       still wouldn't be enough to handle other cases where the list of parents is
       modified (e.g. when [lhs] is invalidated in the example in the comment about
       [can_recompute_now] far below). *)
    for index = 0 to node.num_parents - 1 do
      Stack.push t.propagate_invalidity (Node.get_parent node ~index)
    done;
    if debug then assert (not (Node.needs_to_be_computed node));
    if Node.is_in_recompute_heap node then Recompute_heap.remove t.recompute_heap node)

and invalidate_nodes_created_on_rhs node =
  let r = ref node in
  while Uopt.is_some !r do
    let (T node_on_rhs) = Uopt.unsafe_value !r in
    r := node_on_rhs.next_node_in_same_scope;
    node_on_rhs.next_node_in_same_scope <- Uopt.none;
    invalidate_node node_on_rhs
  done
;;

(* When [not t.bind_lhs_change_should_invalidate_rhs] and a bind's lhs changes, we move
   nodes created on the bind's rhs up to its parent bind, as opposed to [Scope.Top].  This
   maintains their dependence on valid bind left-hand sides, and keeps them higher in the
   graph.  This in turn means that we will continue to compute those nodes after the
   parent bind's lhs, which gives them more of a chance to become unnecessary and not be
   computed should the parent bind's lhs change. *)
let rescope_nodes_created_on_rhs _t (first_node_on_rhs : Node.Packed.t Uopt.t) ~new_scope =
  let r = ref first_node_on_rhs in
  while Uopt.is_some !r do
    let (T node_on_rhs) = Uopt.unsafe_value !r in
    r := node_on_rhs.next_node_in_same_scope;
    node_on_rhs.next_node_in_same_scope <- Uopt.none;
    node_on_rhs.created_in <- new_scope;
    Scope.add_node new_scope node_on_rhs
  done
;;

let propagate_invalidity t =
  while not (Stack.is_empty t.propagate_invalidity) do
    let (T node) = Stack.pop_exn t.propagate_invalidity in
    if Node.is_valid node
    then
      if Node.should_be_invalidated node
      then invalidate_node node
      else (
        (* [Node.needs_to_be_computed node] is true because
           - node is necessary. This is because children can only point to necessary
             parents
           - node is stale. This is because: For bind, if, join, this is true because
           - either the invalidation is caused by the lhs changing (in which case the
             lhs-change node being newer makes us stale).
           - or a child became invalid this stabilization cycle, in which case it has
             t.changed_at of [t.stabilization_num], and so [node] is stale
           - or [node] just became necessary and tried connecting to an already invalid
             child. In that case, [child.changed_at > node.recomputed_at] for that child,
             because if we had been recomputed when that child changed, we would have been
             made invalid back then.  For expert nodes, the argument is the same, except
             that instead of lhs-change nodes make the expert nodes stale, it's made stale
             explicitely when adding or removing children. *)
        if debug then assert (Node.needs_to_be_computed node);
        (match node.kind with
         | Expert expert ->
           (* If multiple children are invalid, they will push us as many times on the
              propagation stack, so we count them right. *)
           Expert.incr_invalid_children expert
         | kind ->
           if debug
           then (
             match kind with
             | Bind_main _ | If_then_else _ | Join_main _ -> ()
             | _ -> assert false (* nodes with no children are never pushed on the stack *)));
        (* We do not check [Node.needs_to_be_computed node] here, because it should be
           true, and because computing it takes O(number of children), node can be pushed
           on the stack once per child, and expert nodes can have lots of children. *)
        if not (Node.is_in_recompute_heap node)
        then Recompute_heap.add t.recompute_heap node)
  done
;;

(* [add_parent_without_adjusting_heights t ~child ~parent] adds [parent] as a parent of
   [child], and makes [child] and all its descendants necessary, ensuring their heights
   are accurate.  There is no guarantee about the relative heights of [child] and [parent]
   though. *)
let rec add_parent_without_adjusting_heights
  : type a b. child:a Node.t -> parent:b Node.t -> child_index:int -> unit
  =
  fun ~child ~parent ~child_index ->
  if debug then assert (Node.is_necessary parent);
  let t = child.state in
  let was_necessary = Node.is_necessary child in
  Node.add_parent ~child ~parent ~child_index;
  if not (Node.is_valid child) then Stack.push t.propagate_invalidity (T parent);
  if not was_necessary then became_necessary child;
  match parent.kind with
  | Expert e -> Expert.run_edge_callback e ~child_index
  | _ -> ()

and became_necessary : type a. a Node.t -> unit =
  fun node ->
  (* [Scope.is_necessary node.created_in] is true (assuming the scope itself is valid)
     because [Node.iter_children] below first visits the lhs-change of bind nodes and
     then the rhs. *)
  if Node.is_valid node && not (Scope.is_necessary node.created_in)
  then
    failwiths
      ~here:[%here]
      "Trying to make a node necessary whose defining bind is not necessary"
      node
      [%sexp_of: _ Node.t];
  let t = node.state in
  t.num_nodes_became_necessary <- t.num_nodes_became_necessary + 1;
  if node.num_on_update_handlers > 0 then handle_after_stabilization node;
  (* Since [node] became necessary, to restore the invariant, we need to:

     - add parent pointers to [node] from its children.
     - set [node]'s height.
     - add [node] to the recompute heap, if necessary. *)
  set_height node (Scope.height node.created_in + 1);
  Node.iteri_children node ~f:(fun child_index (T child) ->
    add_parent_without_adjusting_heights ~child ~parent:node ~child_index;
    (* Now that child is necessary, it should have a valid height. *)
    if debug then assert (child.height >= 0);
    if child.height >= node.height then set_height node (child.height + 1));
  (* Now that the height is correct, maybe add [node] to the recompute heap.  [node]
     just became necessary, so it can't have been in the recompute heap.  Since [node]
     is necessary, we should add it to the recompute heap iff it is stale. *)
  if debug then assert (not (Node.is_in_recompute_heap node));
  if debug then assert (Node.is_necessary node);
  if Node.is_stale node then Recompute_heap.add t.recompute_heap node;
  match node.kind with
  | Expert p -> Expert.observability_change p ~is_now_observable:true
  | _ -> ()
;;

let became_necessary node =
  became_necessary node;
  propagate_invalidity node.state
;;

let add_parent ~child ~parent ~child_index =
  if debug then assert (Node.is_necessary parent);
  let t = parent.state in
  (* In the case when the edge being added creates a cycle, it is possible for the
     recursion in [add_parent_without_adjusting_heights] to reach [parent] as a descendant
     of [child].  In that case, the recursion terminates, because [Node.is_necessary
     parent].  We then return here and subsequently detect the cycle in
     [adjust_heights]. *)
  add_parent_without_adjusting_heights ~child ~parent ~child_index;
  (* We adjust heights so that we ensure there are no cycles before calling
     [propagate_invalidity]. *)
  if child.height >= parent.height
  then
    Adjust_heights_heap.adjust_heights
      t.adjust_heights_heap
      t.recompute_heap
      ~child
      ~parent;
  propagate_invalidity t;
  if debug then assert (Node.is_necessary parent);
  (* we only add necessary parents *)
  if (not (Node.is_in_recompute_heap parent))
  && (Stabilization_num.is_none parent.recomputed_at
      || Node.edge_is_stale ~child ~parent)
  then Recompute_heap.add t.recompute_heap parent
;;

let run_with_scope t scope ~f =
  let saved = t.current_scope in
  t.current_scope <- scope;
  try
    let v = f () in
    t.current_scope <- saved;
    v
  with
  | exn ->
    t.current_scope <- saved;
    raise exn
;;

let within_scope t scope ~f =
  if not (Scope.is_valid scope)
  then failwiths ~here:[%here] "attempt to run within an invalid scope" t [%sexp_of: t];
  run_with_scope t scope ~f
;;

let change_child
  : type a b.
    parent:a Node.t
    -> old_child:b Node.t Uopt.t
    -> new_child:b Node.t
    -> child_index:int
    -> unit
  =
  fun ~parent ~old_child ~new_child ~child_index ->
  if Uopt.is_none old_child
  then add_parent ~child:new_child ~parent ~child_index
  else (
    let old_child = Uopt.unsafe_value old_child in
    if not (phys_equal old_child new_child)
    then (
      (* We remove [old_child] before adding [new_child], because they share the same
         child index. *)
      Node.remove_parent ~child:old_child ~parent ~child_index;
      (* We force [old_child] to temporarily be necessary so that [add_parent] can't
         mistakenly think it is unnecessary and transition it to necessary (which would
         add duplicate edges and break things horribly). *)
      old_child.force_necessary <- true;
      add_parent ~child:new_child ~parent ~child_index;
      old_child.force_necessary <- false;
      (* We [check_if_unnecessary] after [add_parent], so that we don't unnecessarily
         transition nodes from necessary to unnecessary and then back again. *)
      check_if_unnecessary old_child))
;;

let add_alarm clock ~at alarm_value =
  if debug then assert (Time_ns.( > ) at (now clock));
  Timing_wheel.add clock.timing_wheel ~at alarm_value
;;

let rec recompute : type a. a Node.t -> unit =
  fun node ->
  let t = node.state in
  if debug
  then (
    t.only_in_debug.currently_running_node <- Some (T node);
    t.only_in_debug.expert_nodes_created_by_current_node <- []);
  t.num_nodes_recomputed <- t.num_nodes_recomputed + 1;
  node.recomputed_at <- t.stabilization_num;
  match node.kind with
  | Array_fold array_fold -> maybe_change_value node (Array_fold.compute array_fold)
  | At { at; clock; _ } ->
    (* It is a bug if we try to compute an [At] node after [at].  [advance_clock] was
       supposed to convert it to a [Const] at the appropriate time. *)
    if debug then assert (Time_ns.( > ) at (now clock));
    maybe_change_value node Before
  | At_intervals _ -> maybe_change_value node ()
  | Bind_lhs_change
      ({ main
       ; f
       ; lhs
       ; rhs_scope
       ; rhs = old_rhs
       ; all_nodes_created_on_rhs = old_all_nodes_created_on_rhs
       ; _
       } as bind) ->
    (* We clear [all_nodes_created_on_rhs] so it will hold just the nodes created by
       this call to [f]. *)
    bind.all_nodes_created_on_rhs <- Uopt.none;
    let rhs = run_with_scope t rhs_scope ~f:(fun () -> f (Node.value_exn lhs)) in
    bind.rhs <- Uopt.some rhs;
    (* Anticipate what [maybe_change_value] will do, to make sure Bind_main is stale
       right away. This way, if the new child is invalid, we'll satisfy the invariant
       saying that [needs_to_be_computed bind_main] in [propagate_invalidity] *)
    node.changed_at <- t.stabilization_num;
    change_child
      ~parent:main
      ~old_child:old_rhs
      ~new_child:rhs
      ~child_index:Kind.bind_rhs_child_index;
    if Uopt.is_some old_rhs
    then (
      (* We invalidate after [change_child], because invalidation changes the [kind] of
         nodes to [Invalid], which means that we can no longer visit their children.
         Also, the [old_rhs] nodes are typically made unnecessary by [change_child], and
         so by invalidating afterwards, we will not waste time adding them to the
         recompute heap and then removing them. *)
      if t.bind_lhs_change_should_invalidate_rhs
      then invalidate_nodes_created_on_rhs old_all_nodes_created_on_rhs
      else
        rescope_nodes_created_on_rhs
          t
          old_all_nodes_created_on_rhs
          ~new_scope:main.created_in;
      propagate_invalidity t);
    (* [node] was valid at the start of the [Bind_lhs_change] branch, and invalidation
       only visits higher nodes, so [node] is still valid. *)
    if debug then assert (Node.is_valid node);
    maybe_change_value node ()
  | Bind_main { rhs; _ } -> copy_child ~parent:node ~child:(Uopt.value_exn rhs)
  | Const a -> maybe_change_value node a
  | Freeze { child; only_freeze_when; _ } ->
    let value = Node.value_exn child in
    if only_freeze_when value
    then (
      remove_children node;
      Node.set_kind node (Const value);
      if Node.is_necessary node then set_height node 0 else became_unnecessary node);
    maybe_change_value node value
  | If_test_change ({ main; current_branch; test; then_; else_; _ } as if_then_else) ->
    let desired_branch = if Node.value_exn test then then_ else else_ in
    if_then_else.current_branch <- Uopt.some desired_branch;
    (* see the comment in Bind_lhs_change *)
    node.changed_at <- t.stabilization_num;
    change_child
      ~parent:main
      ~old_child:current_branch
      ~new_child:desired_branch
      ~child_index:Kind.if_branch_child_index;
    maybe_change_value node ()
  | If_then_else { current_branch; _ } ->
    copy_child ~parent:node ~child:(Uopt.value_exn current_branch)
  | Invalid ->
    (* We never have invalid nodes in the recompute heap; they are never stale. *)
    assert false
  | Join_lhs_change ({ lhs; main; rhs = old_rhs; _ } as join) ->
    let rhs = Node.value_exn lhs in
    join.rhs <- Uopt.some rhs;
    (* see the comment in Bind_lhs_change *)
    node.changed_at <- t.stabilization_num;
    change_child
      ~parent:main
      ~old_child:old_rhs
      ~new_child:rhs
      ~child_index:Kind.join_rhs_child_index;
    maybe_change_value node ()
  | Join_main { rhs; _ } -> copy_child ~parent:node ~child:(Uopt.value_exn rhs)
  | Map (f, n1) -> maybe_change_value node (f (Node.value_exn n1))
  | Snapshot { at; before; clock; _ } ->
    (* It is a bug if we try to compute a [Snapshot] and the alarm should have fired.
       [advance_clock] was supposed to convert it to a [Freeze] at the appropriate
       time. *)
    if debug then assert (Time_ns.( > ) at (now clock));
    maybe_change_value node before
  | Step_function ({ child; clock; _ } as step_function_node) ->
    if Uopt.is_some child
    then (
      let child = Uopt.value_exn child in
      if Stabilization_num.compare
           child.changed_at
           step_function_node.extracted_step_function_from_child_at
         > 0
      then (
        step_function_node.extracted_step_function_from_child_at <- child.changed_at;
        remove_alarm clock step_function_node.alarm;
        let step_function = Node.value_exn child in
        step_function_node.value <- Uopt.some (Step_function.init step_function);
        step_function_node.upcoming_steps <- Step_function.steps step_function;
        (* If the child is a constant, we drop our reference to it, to avoid holding on to
           the entire step function. *)
        if Node.is_const child
        then (
          remove_children node;
          step_function_node.child <- Uopt.none;
          set_height node (Scope.height node.created_in + 1))));
    Step_function_node.advance step_function_node ~to_:(now clock);
    let step_function_value = Uopt.value_exn step_function_node.value in
    (match Sequence.hd step_function_node.upcoming_steps with
     | None -> if Uopt.is_none child then Node.set_kind node (Const step_function_value)
     | Some (at, _) ->
       step_function_node.alarm <- add_alarm clock ~at step_function_node.alarm_value);
    maybe_change_value node step_function_value
  | Unordered_array_fold u -> maybe_change_value node (Unordered_array_fold.compute u)
  | Uninitialized -> assert false
  | Var var -> maybe_change_value node var.value
  | Map2 (f, n1, n2) ->
    maybe_change_value node (f (Node.value_exn n1) (Node.value_exn n2))
  | Map3 (f, n1, n2, n3) ->
    maybe_change_value
      node
      (f (Node.value_exn n1) (Node.value_exn n2) (Node.value_exn n3))
  | Map4 (f, n1, n2, n3, n4) ->
    maybe_change_value
      node
      (f (Node.value_exn n1) (Node.value_exn n2) (Node.value_exn n3) (Node.value_exn n4))
  | Map5 (f, n1, n2, n3, n4, n5) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5))
  | Map6 (f, n1, n2, n3, n4, n5, n6) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6))
  | Map7 (f, n1, n2, n3, n4, n5, n6, n7) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7))
  | Map8 (f, n1, n2, n3, n4, n5, n6, n7, n8) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8))
  | Map9 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9))
  | Map10 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9)
         (Node.value_exn n10))
  | Map11 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9)
         (Node.value_exn n10)
         (Node.value_exn n11))
  | Map12 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9)
         (Node.value_exn n10)
         (Node.value_exn n11)
         (Node.value_exn n12))
  | Map13 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9)
         (Node.value_exn n10)
         (Node.value_exn n11)
         (Node.value_exn n12)
         (Node.value_exn n13))
  | Map14 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9)
         (Node.value_exn n10)
         (Node.value_exn n11)
         (Node.value_exn n12)
         (Node.value_exn n13)
         (Node.value_exn n14))
  | Map15 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15) ->
    maybe_change_value
      node
      (f
         (Node.value_exn n1)
         (Node.value_exn n2)
         (Node.value_exn n3)
         (Node.value_exn n4)
         (Node.value_exn n5)
         (Node.value_exn n6)
         (Node.value_exn n7)
         (Node.value_exn n8)
         (Node.value_exn n9)
         (Node.value_exn n10)
         (Node.value_exn n11)
         (Node.value_exn n12)
         (Node.value_exn n13)
         (Node.value_exn n14)
         (Node.value_exn n15))
  | Expert expert ->
    (match Expert.before_main_computation expert with
     | `Invalid ->
       invalidate_node node;
       propagate_invalidity t
     | `Ok -> maybe_change_value node (expert.f ()))

and copy_child : type a. parent:a Node.t -> child:a Node.t -> unit =
  fun ~parent ~child ->
  if Node.is_valid child
  then maybe_change_value parent (Node.value_exn child)
  else (
    invalidate_node parent;
    propagate_invalidity parent.state)

and maybe_change_value : type a. a Node.t -> a -> unit =
  fun node new_value ->
  let t = node.state in
  let old_value_opt = node.value_opt in
  if Uopt.is_none old_value_opt
  || not
       (Cutoff.should_cutoff
          node.cutoff
          ~old_value:(Uopt.unsafe_value old_value_opt)
          ~new_value)
  then (
    node.value_opt <- Uopt.some new_value;
    node.changed_at <- t.stabilization_num;
    t.num_nodes_changed <- t.num_nodes_changed + 1;
    if node.num_on_update_handlers > 0
    then (
      node.old_value_opt <- old_value_opt;
      handle_after_stabilization node);
    if node.num_parents >= 1
    then (
      for parent_index = 1 to node.num_parents - 1 do
        let (T parent) = Uopt.value_exn node.parent1_and_beyond.(parent_index - 1) in
        (match parent.kind with
         | Expert expert ->
           let child_index = node.my_child_index_in_parent_at_index.(parent_index) in
           Expert.run_edge_callback ~child_index expert
         | Unordered_array_fold u ->
           Unordered_array_fold.child_changed
             u
             ~child:node
             ~child_index:node.my_child_index_in_parent_at_index.(parent_index)
             ~old_value_opt
             ~new_value
         | _ -> ());
        if debug then assert (Node.needs_to_be_computed parent);
        (* We don't do the [can_recompute_now] optimization.  Since most nodes only have
           one parent, it is not probably not a big loss.  If we did it anyway, we'd
           have to be careful, because while we iterate over the list of parents, we
           would execute them, and in particular we can execute lhs-change nodes who can
           change the structure of the list of parents we iterate on.  Think about:

           {[
             lhs >>= fun b -> if b then lhs >>| Fn.id else const b
           ]}

           If the optimization kicks in when we propagate change to the parents of [lhs]
           (which changes from [true] to [false]), we could execute the [lhs-change]
           first, which would make disconnect the [map] node from [lhs].  And then we
           would execute the second child of the [lhs], which doesn't exist anymore and
           incremental would segfault (there may be a less naive way of making this work
           though). *)
        if not (Node.is_in_recompute_heap parent)
        then Recompute_heap.add t.recompute_heap parent
      done;
      let (T parent) = Uopt.value_exn node.parent0 in
      (match parent.kind with
       | Expert p ->
         let child_index = node.my_child_index_in_parent_at_index.(0) in
         Expert.run_edge_callback ~child_index p
       | Unordered_array_fold u ->
         Unordered_array_fold.child_changed
           u
           ~child:node
           ~child_index:node.my_child_index_in_parent_at_index.(0)
           ~old_value_opt
           ~new_value
       | _ -> ());
      if debug then assert (Node.needs_to_be_computed parent);
      if not (Node.is_in_recompute_heap parent)
      then (
        let can_recompute_now =
          match parent.kind with
          | Uninitialized -> assert false
          (* These nodes aren't parents. *)
          | At _ -> assert false
          | At_intervals _ -> assert false
          | Const _ | Invalid | Snapshot _ | Var _ -> assert false
          (* These nodes have more than one child. *)
          | Array_fold _
          | Map2 _
          | Map3 _
          | Map4 _
          | Map5 _
          | Map6 _
          | Map7 _
          | Map8 _
          | Map9 _
          | Map10 _
          | Map11 _
          | Map12 _
          | Map13 _
          | Map14 _
          | Map15 _
          | Unordered_array_fold _
          | Expert _ -> false
          (* We can immediately recompute [parent] if no other node needs to be stable
             before computing it.  If [parent] has a single child (i.e. [node]), then
             this amounts to checking that [parent] won't be invalidated, i.e. that
             [parent]'s scope has already stabilized. *)
          | Bind_lhs_change _ -> node.height > Scope.height parent.created_in
          | Freeze _ -> node.height > Scope.height parent.created_in
          | If_test_change _ -> node.height > Scope.height parent.created_in
          | Join_lhs_change _ -> node.height > Scope.height parent.created_in
          | Map _ -> node.height > Scope.height parent.created_in
          | Step_function _ -> node.height > Scope.height parent.created_in
          (* For these, we need to check that the "_change" child has already been
             evaluated (if needed).  If so, this also implies:

             {[
               node.height > Scope.height parent.created_in
             ]} *)
          | Bind_main b -> node.height > b.lhs_change.height
          | If_then_else i -> node.height > i.test_change.height
          | Join_main j -> node.height > j.lhs_change.height
        in
        if can_recompute_now
        then (
          t.num_nodes_recomputed_directly_because_one_child
          <- t.num_nodes_recomputed_directly_because_one_child + 1;
          recompute parent)
        else if parent.height <= Recompute_heap.min_height t.recompute_heap
        then (
          (* If [parent.height] is [<=] the height of all nodes in the recompute heap
             (possibly because the recompute heap is empty), then we can recompute
             [parent] immediately and save adding it to and then removing it from the
             recompute heap. *)
          t.num_nodes_recomputed_directly_because_min_height
          <- t.num_nodes_recomputed_directly_because_min_height + 1;
          recompute parent)
        else (
          if debug then assert (Node.needs_to_be_computed parent);
          if debug then assert (not (Node.is_in_recompute_heap parent));
          Recompute_heap.add t.recompute_heap parent))));
  if debug then invariant t
;;

let recompute_everything_that_is_necessary t =
  let module R = Recompute_heap in
  let r = t.recompute_heap in
  while R.length r > 0 do
    let (T node) = R.remove_min r in
    if debug && not (Node.needs_to_be_computed node)
    then
      failwiths
        ~here:[%here]
        "node unexpectedly does not need to be computed"
        node
        [%sexp_of: _ Node.t];
    recompute node
  done;
  if debug
  then (
    t.only_in_debug.currently_running_node <- None;
    t.only_in_debug.expert_nodes_created_by_current_node <- [])
;;

let unlink_disallowed_observers t =
  while Stack.length t.disallowed_observers > 0 do
    let packed = Stack.pop_exn t.disallowed_observers in
    let module Packed = Internal_observer.Packed in
    let (T internal_observer) = packed in
    if debug
    then
      assert (
        match internal_observer.state with
        | Disallowed -> true
        | _ -> false);
    internal_observer.state <- Unlinked;
    let (T all_observers) = Uopt.value_exn t.all_observers in
    if Internal_observer.same internal_observer all_observers
    then t.all_observers <- internal_observer.next_in_all;
    Internal_observer.unlink internal_observer;
    check_if_unnecessary internal_observer.observing
  done
;;

let disallow_future_use internal_observer =
  let t = Internal_observer.incr_state internal_observer in
  match internal_observer.state with
  | Disallowed | Unlinked -> ()
  | Created ->
    t.num_active_observers <- t.num_active_observers - 1;
    internal_observer.state <- Unlinked;
    internal_observer.on_update_handlers <- []
  | In_use ->
    t.num_active_observers <- t.num_active_observers - 1;
    internal_observer.state <- Disallowed;
    Stack.push t.disallowed_observers (T internal_observer)
;;

let disallow_finalized_observers t =
  while Thread_safe_queue.length t.finalized_observers > 0 do
    let (T internal_observer) = Thread_safe_queue.dequeue_exn t.finalized_observers in
    if List.is_empty internal_observer.on_update_handlers
    then disallow_future_use internal_observer
  done
;;

let observer_finalizer t =
  stage (fun observer ->
    let internal_observer = !observer in
    Thread_safe_queue.enqueue t.finalized_observers (T internal_observer))
;;

let create_observer ?(should_finalize = true) (observing : _ Node.t) =
  let t = observing.state in
  let internal_observer : _ Internal_observer.t =
    { state = Created
    ; observing
    ; on_update_handlers = []
    ; prev_in_all = Uopt.none
    ; next_in_all = Uopt.none
    ; prev_in_observing = Uopt.none
    ; next_in_observing = Uopt.none
    }
  in
  Stack.push t.new_observers (T internal_observer);
  let observer = ref internal_observer in
  if should_finalize
  then Gc.Expert.add_finalizer_exn observer (unstage (observer_finalizer t));
  t.num_active_observers <- t.num_active_observers + 1;
  observer
;;

let add_new_observers t =
  while Stack.length t.new_observers > 0 do
    let packed = Stack.pop_exn t.new_observers in
    let module Packed = Internal_observer.Packed in
    let (T internal_observer) = packed in
    match internal_observer.state with
    | In_use | Disallowed -> assert false
    | Unlinked -> ()
    | Created ->
      internal_observer.state <- In_use;
      let old_all_observers = t.all_observers in
      if Uopt.is_some old_all_observers
      then (
        internal_observer.next_in_all <- old_all_observers;
        Packed.set_prev_in_all (Uopt.unsafe_value old_all_observers) (Uopt.some packed));
      t.all_observers <- Uopt.some packed;
      let observing = internal_observer.observing in
      let was_necessary = Node.is_necessary observing in
      observing.num_on_update_handlers
      <- observing.num_on_update_handlers
         + List.length internal_observer.on_update_handlers;
      let old_observers = observing.observers in
      if Uopt.is_some old_observers
      then (
        internal_observer.next_in_observing <- old_observers;
        (Uopt.unsafe_value old_observers).prev_in_observing <- Uopt.some internal_observer);
      observing.observers <- Uopt.some internal_observer;
      (* By adding [internal_observer] to [observing.observers], we may have added
         on-update handlers to [observing].  We need to handle [observing] after this
         stabilization to give those handlers a chance to run. *)
      handle_after_stabilization observing;
      if debug then assert (Node.is_necessary observing);
      if not was_necessary then became_necessary observing
  done
;;

let observer_value_exn observer =
  let t = Observer.incr_state observer in
  match t.status with
  | Not_stabilizing | Running_on_update_handlers -> Observer.value_exn observer
  | Stabilize_previously_raised raised_exn ->
    failwiths
      ~here:[%here]
      "Observer.value_exn called after stabilize previously raised"
      raised_exn
      [%sexp_of: Raised_exn.t]
  | Stabilizing ->
    failwiths
      ~here:[%here]
      "Observer.value_exn called during stabilization"
      observer
      [%sexp_of: _ Observer.t]
;;

let observer_value observer =
  try Ok (observer_value_exn observer) with
  | exn -> Error (Error.of_exn exn)
;;

let node_on_update (type a) (node : a Node.t) ~f =
  let t = node.state in
  Node.on_update node (On_update_handler.create f ~at:t.stabilization_num);
  handle_after_stabilization node
;;

let observer_on_update_exn observer ~f =
  let t = Observer.incr_state observer in
  Observer.on_update_exn observer (On_update_handler.create f ~at:t.stabilization_num);
  handle_after_stabilization (Observer.observing observer)
;;

let set_var_while_not_stabilizing var value =
  let t = Var.incr_state var in
  t.num_var_sets <- t.num_var_sets + 1;
  var.value <- value;
  if Stabilization_num.compare var.set_at t.stabilization_num < 0
  then (
    var.set_at <- t.stabilization_num;
    let watch = var.watch in
    if debug then assert (Node.is_stale watch);
    if Node.is_necessary watch && not (Node.is_in_recompute_heap watch)
    then Recompute_heap.add t.recompute_heap watch)
;;

let set_var var value =
  let t = Var.incr_state var in
  match t.status with
  | Running_on_update_handlers | Not_stabilizing ->
    set_var_while_not_stabilizing var value
  | Stabilize_previously_raised raised_exn ->
    failwiths
      ~here:[%here]
      "cannot set var -- stabilization previously raised"
      raised_exn
      [%sexp_of: Raised_exn.t]
  | Stabilizing ->
    if Uopt.is_none var.value_set_during_stabilization
    then Stack.push t.set_during_stabilization (T var);
    var.value_set_during_stabilization <- Uopt.some value
;;

let reclaim_space_in_weak_hashtbls t =
  while Thread_safe_queue.length t.weak_hashtbls > 0 do
    let (T weak_hashtbl) = Thread_safe_queue.dequeue_exn t.weak_hashtbls in
    Weak_hashtbl.reclaim_space_for_keys_with_unused_data weak_hashtbl
  done
;;

let stabilize t =
  ensure_not_stabilizing t ~name:"stabilize" ~allow_in_update_handler:false;
  try
    t.status <- Stabilizing;
    disallow_finalized_observers t;
    (* Just like for binds, we add new observers before removing disallowed observers to
       potentially avoid switching the observability of some nodes back and forth. *)
    add_new_observers t;
    unlink_disallowed_observers t;
    if debug then invariant t;
    recompute_everything_that_is_necessary t;
    (* We increment [t.stabilization_num] before handling variables set during
       stabilization, so that they are treated as set during the new stabilization cycle.
       Also, we increment before running on-update handlers, to avoid running on update
       handlers created during on update handlers. *)
    t.stabilization_num <- Stabilization_num.add1 t.stabilization_num;
    while not (Stack.is_empty t.set_during_stabilization) do
      let (T var) = Stack.pop_exn t.set_during_stabilization in
      let value = Uopt.value_exn var.value_set_during_stabilization in
      var.value_set_during_stabilization <- Uopt.none;
      set_var_while_not_stabilizing var value
    done;
    while not (Stack.is_empty t.handle_after_stabilization) do
      let (T node) = Stack.pop_exn t.handle_after_stabilization in
      node.is_in_handle_after_stabilization <- false;
      let old_value = node.old_value_opt in
      node.old_value_opt <- Uopt.none;
      let node_update : _ Node_update.t =
        if not (Node.is_valid node)
        then Invalidated
        else if not (Node.is_necessary node)
        then Unnecessary
        else (
          let new_value = Uopt.value_exn node.value_opt in
          if Uopt.is_none old_value
          then Necessary new_value
          else Changed (Uopt.unsafe_value old_value, new_value))
      in
      Stack.push t.run_on_update_handlers (T (node, node_update))
    done;
    t.status <- Running_on_update_handlers;
    let now = t.stabilization_num in
    while not (Stack.is_empty t.run_on_update_handlers) do
      let (T (node, node_update)) = Stack.pop_exn t.run_on_update_handlers in
      Node.run_on_update_handlers node node_update ~now
    done;
    t.status <- Not_stabilizing;
    reclaim_space_in_weak_hashtbls t
  with
  | exn ->
    t.status <- Stabilize_previously_raised (Raised_exn.create exn);
    raise exn
;;

let create_node_in t created_in kind =
  t.num_nodes_created <- t.num_nodes_created + 1;
  Node.create t created_in kind
;;

let create_node t kind = create_node_in t t.current_scope kind
let create_node_top t kind = create_node_in t Scope.top kind

let create_var t ?(use_current_scope = false) value =
  let scope = if use_current_scope then t.current_scope else Scope.top in
  let watch = create_node_in t scope Uninitialized in
  let var =
    { Var.value
    ; value_set_during_stabilization = Uopt.none
    ; set_at = t.stabilization_num
    ; watch
    }
  in
  Node.set_kind watch (Var var);
  var
;;

(* A [const] value could come from the right-hand side of an outer bind.  So, we create a
   [const] node in the current scope, not in [Scope.top]. *)
let const t a = create_node t (Const a)
let map (n : _ Node.t) ~f = create_node n.state (Map (f, n))
let map2 (n1 : _ Node.t) n2 ~f = create_node n1.state (Map2 (f, n1, n2))
let map3 (n1 : _ Node.t) n2 n3 ~f = create_node n1.state (Map3 (f, n1, n2, n3))
let map4 (n1 : _ Node.t) n2 n3 n4 ~f = create_node n1.state (Map4 (f, n1, n2, n3, n4))

let map5 (n1 : _ Node.t) n2 n3 n4 n5 ~f =
  create_node n1.state (Map5 (f, n1, n2, n3, n4, n5))
;;

let map6 (n1 : _ Node.t) n2 n3 n4 n5 n6 ~f =
  create_node n1.state (Map6 (f, n1, n2, n3, n4, n5, n6))
;;

let map7 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 ~f =
  create_node n1.state (Map7 (f, n1, n2, n3, n4, n5, n6, n7))
;;

let map8 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 ~f =
  create_node n1.state (Map8 (f, n1, n2, n3, n4, n5, n6, n7, n8))
;;

let map9 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 ~f =
  create_node n1.state (Map9 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9))
;;

let map10 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 ~f =
  create_node n1.state (Map10 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10))
;;

let map11 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 ~f =
  create_node n1.state (Map11 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11))
;;

let map12 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 ~f =
  create_node n1.state (Map12 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12))
;;

let map13 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 ~f =
  create_node n1.state (Map13 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13))
;;

let map14 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 ~f =
  create_node
    n1.state
    (Map14 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14))
;;

let map15 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 ~f =
  create_node
    n1.state
    (Map15 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15))
;;

let preserve_cutoff ~(input : _ Node.t) ~output =
  Node.set_cutoff
    output
    (Cutoff.create (fun ~old_value:_ ~new_value:_ ->
       Stabilization_num.equal input.changed_at output.changed_at))
;;

let depend_on input ~depend_on =
  let output = map2 input depend_on ~f:(fun a _ -> a) in
  preserve_cutoff ~input ~output;
  output
;;

let necessary_if_alive input =
  (* If [output] is alive, then [observer] is alive, then [input] is necessary.  If
     [output] is unnecessary, then [output] is not a parent of [input], and thus
     [output]'s liveness is dependent solely on user code.  And in particular, if [output]
     dies, then [observer] will be finalized, and then upon the next stabilization,
     [input] will become unnecessary (at least with respect to [output]). *)
  let observer = create_observer input in
  let output =
    map input ~f:(fun a ->
      Gc.keep_alive observer;
      a)
  in
  preserve_cutoff ~input ~output;
  output
;;

let bind (lhs : _ Node.t) ~f =
  let t = lhs.state in
  let lhs_change = create_node t Uninitialized in
  let main = create_node t Uninitialized in
  let bind =
    { Bind.main
    ; f
    ; lhs
    ; lhs_change
    ; rhs = Uopt.none
    ; rhs_scope = Scope.top
    ; all_nodes_created_on_rhs = Uopt.none
    }
  in
  (* We set [lhs_change] to never cutoff so that whenever [lhs] changes, [main] is
     recomputed.  This is necessary to handle cases where [f] returns an existing stable
     node, in which case the [lhs_change] would be the only thing causing [main] to be
     stale. *)
  Node.set_cutoff lhs_change Cutoff.never;
  bind.rhs_scope <- Bind bind;
  Node.set_kind lhs_change (Bind_lhs_change bind);
  Node.set_kind main (Bind_main bind);
  main
;;

let bind2 n1 n2 ~f =
  bind (map2 n1 n2 ~f:(fun v1 v2 -> v1, v2)) ~f:(fun (v1, v2) -> f v1 v2)
;;

let bind3 n1 n2 n3 ~f =
  bind (map3 n1 n2 n3 ~f:(fun v1 v2 v3 -> v1, v2, v3)) ~f:(fun (v1, v2, v3) -> f v1 v2 v3)
;;

let bind4 n1 n2 n3 n4 ~f =
  bind
    (map4 n1 n2 n3 n4 ~f:(fun v1 v2 v3 v4 -> v1, v2, v3, v4))
    ~f:(fun (v1, v2, v3, v4) -> f v1 v2 v3 v4)
;;

let join (lhs : _ Node.t) =
  let t = lhs.state in
  let lhs_change = create_node t Uninitialized in
  let main = create_node t Uninitialized in
  let join = { Join.lhs; lhs_change; rhs = Uopt.none; main } in
  Node.set_cutoff lhs_change Cutoff.never;
  Node.set_kind lhs_change (Join_lhs_change join);
  Node.set_kind main (Join_main join);
  main
;;

let if_ (test : _ Node.t) ~then_ ~else_ =
  let t = test.state in
  let test_change = create_node t Uninitialized in
  let main = create_node t Uninitialized in
  let if_then_else =
    { If_then_else.test; then_; else_; test_change; main; current_branch = Uopt.none }
  in
  Node.set_cutoff test_change Cutoff.never;
  Node.set_kind test_change (If_test_change if_then_else);
  Node.set_kind main (If_then_else if_then_else);
  main
;;

let lazy_from_fun t ~f =
  let scope = t.current_scope in
  Lazy.from_fun (fun () -> within_scope t scope ~f)
;;

let default_hash_table_initial_size = 4

let memoize_fun_by_key
      ?(initial_size = default_hash_table_initial_size)
      t
      hashable
      project_key
      f
  =
  (* Here's an explanation of why we get [t.current_scope] here, and then call
     [within_scope] below.  Consider this (impossible) alternate implementation of
     [memoize_fun_by_key]:

     {[
       let table =
         Hashtbl.of_alist_exn hashable
           (List.map all_possible_a_values ~f:(fun a -> (project_key a, f a))
       in
       stage (fun key -> Hashtbl.find_exn table (project_key a))
     ]}

     This implementation doesn't use [current_scope] or [within_scope].  All calls to [f]
     naturally occur in [t.current_scope].

     Such an implementation is impossible because we do not have [all_possible_a_values].
     The implementation below uses [within_scope] to call [f a] in the scope that was
     current at the point of the call to [memoize_fun_by_key] so that we can think of the
     [table] as having been created then, when it in reality is created on-demand. *)
  let scope = t.current_scope in
  let table = Hashtbl.create hashable ~size:initial_size in
  stage (fun a ->
    let key = project_key a in
    match Hashtbl.find table key with
    | Some b -> b
    | None ->
      let b = within_scope t scope ~f:(fun () -> f a) in
      Hashtbl.add_exn table ~key ~data:b;
      b)
;;

let array_fold t children ~init ~f =
  if Array.length children = 0
  then const t init
  else create_node t (Array_fold { init; f; children })
;;

let all t ts = array_fold t (Array.of_list_rev ts) ~init:[] ~f:(fun ac a -> a :: ac)

module Unordered_array_fold_update = Unordered_array_fold.Update

let unordered_array_fold
      t
      ?(full_compute_every_n_changes = Int.max_value)
      children
      ~init
      ~f
      ~update
  =
  if Array.length children = 0
  then const t init
  else if full_compute_every_n_changes <= 0
  then
    failwiths
      ~here:[%here]
      "unordered_array_fold got non-positive full_compute_every_n_changes"
      full_compute_every_n_changes
      [%sexp_of: int]
  else (
    let main = create_node t Uninitialized in
    Node.set_kind
      main
      (Unordered_array_fold
         (Unordered_array_fold.create
            ~init
            ~f
            ~update
            ~full_compute_every_n_changes
            ~children
            ~main));
    main)
;;

let opt_unordered_array_fold t ?full_compute_every_n_changes ts ~init ~f ~f_inverse =
  let f (accum, num_invalid) x =
    match x with
    | None -> accum, num_invalid + 1
    | Some x -> f accum x, num_invalid
  in
  let f_inverse (accum, num_invalid) x =
    match x with
    | None -> accum, num_invalid - 1
    | Some x -> f_inverse accum x, num_invalid
  in
  map
    (unordered_array_fold
       t
       ts
       ~init:(init, 0)
       ~f
       ~update:(F_inverse f_inverse)
       ?full_compute_every_n_changes)
    ~f:(fun (accum, num_invalid) -> if num_invalid = 0 then Some accum else None)
;;

let at_least_k_of t nodes ~k =
  let bool_to_int b = if b then 1 else 0 in
  map
    ~f:(fun i -> i >= k)
    (unordered_array_fold
       t
       nodes
       ~init:0
       ~f:(fun num_true b -> num_true + bool_to_int b)
       ~update:(F_inverse (fun num_true b -> num_true - bool_to_int b)))
;;

let exists t nodes = at_least_k_of t nodes ~k:1
let for_all t nodes = at_least_k_of t nodes ~k:(Array.length nodes)

let sum t ?full_compute_every_n_changes nodes ~zero ~add ~sub =
  unordered_array_fold
    t
    nodes
    ~init:zero
    ~f:add
    ~update:(F_inverse sub)
    ?full_compute_every_n_changes
;;

let opt_sum t ?full_compute_every_n_changes nodes ~zero ~add ~sub =
  opt_unordered_array_fold
    t
    nodes
    ~init:zero
    ~f:add
    ~f_inverse:sub
    ?full_compute_every_n_changes
;;

let sum_int t nodes = sum t nodes ~zero:0 ~add:( + ) ~sub:( - )

let sum_float t nodes =
  sum
    t
    nodes
    ~zero:0.
    ~add:( +. )
    ~sub:( -. )
    ~full_compute_every_n_changes:(Array.length nodes)
;;

let set_freeze (node : _ Node.t) ~child ~only_freeze_when =
  if debug then assert (Scope.is_top node.created_in);
  (* By making [node.kind] be [Freeze], we are making [Node.is_necessary node]. *)
  let was_necessary = Node.is_necessary node in
  Node.set_kind node (Freeze { main = node; child; only_freeze_when });
  if was_necessary
  then add_parent ~child ~parent:node ~child_index:Kind.freeze_child_index
  else became_necessary node
;;

let freeze (child : _ Node.t) ~only_freeze_when =
  let t = child.state in
  let node = create_node_top t Uninitialized in
  set_freeze node ~child ~only_freeze_when;
  node
;;

let at clock time =
  let t = Clock.incr_state clock in
  if Time_ns.( <= ) time (now clock)
  then const t Before_or_after.After
  else (
    let main = create_node t Uninitialized in
    let at = { At.at = time; main; alarm = Alarm.null; clock } in
    Node.set_kind main (At at);
    at.alarm <- add_alarm clock ~at:time (Alarm_value.create (At at));
    main)
;;

let after clock span = at clock (Time_ns.add (now clock) span)

let next_interval_alarm_strict (clock : Clock.t) ~base ~interval =
  let after = now clock in
  let at = Time_ns.next_multiple ~base ~after ~interval ~can_equal_after:false () in
  if debug then assert (Time_ns.( > ) at after);
  at
;;

let at_intervals (clock : Clock.t) interval =
  let t = Clock.incr_state clock in
  if Time_ns.Span.( < ) interval (Timing_wheel.alarm_precision clock.timing_wheel)
  then
    failwiths
      ~here:[%here]
      "at_intervals got too small interval"
      interval
      [%sexp_of: Time_ns.Span.t];
  let main = create_node t Uninitialized in
  let base = now clock in
  let at_intervals = { At_intervals.main; base; interval; alarm = Alarm.null; clock } in
  Node.set_kind main (At_intervals at_intervals);
  (* [main : unit Node.t], so we make it never cutoff so it changes each time it is
     recomputed. *)
  Node.set_cutoff main Cutoff.never;
  at_intervals.alarm
  <- add_alarm
       clock
       ~at:(next_interval_alarm_strict clock ~base ~interval)
       (Alarm_value.create (At_intervals at_intervals));
  main
;;

let snapshot clock value_at ~at ~before =
  let t = Clock.incr_state clock in
  if Time_ns.( <= ) at (now clock)
  then
    if Time_ns.( < ) at (now clock)
    then Or_error.error "cannot take snapshot in the past" at [%sexp_of: Time_ns.t]
    else Ok (freeze value_at ~only_freeze_when:(Fn.const true))
  else (
    let main = create_node_top t Uninitialized in
    let snapshot = { Snapshot.main; at; before; value_at; clock } in
    Node.set_kind main (Snapshot snapshot);
    (* Unlike other time-based incrementals, a snapshot is created in [Scope.top] and
       cannot be invalidated by its scope.  Thus, there is no need to keep track of the
       alarm that is added, because it will never need to be removed early. *)
    ignore (add_alarm clock ~at (Alarm_value.create (Snapshot snapshot)) : Alarm.t);
    Ok main)
;;

let incremental_step_function clock child =
  let t = Clock.incr_state clock in
  let main = create_node t Uninitialized in
  let step_function_node =
    { Step_function_node.main
    ; value = Uopt.none
    ; child = Uopt.some child
    ; extracted_step_function_from_child_at = Stabilization_num.none
    ; upcoming_steps = Sequence.empty
    ; alarm = Alarm.null
    ; alarm_value = Obj.magic None (* set below *)
    ; clock
    }
  in
  step_function_node.alarm_value <- Alarm_value.create (Step_function step_function_node);
  Node.set_kind main (Step_function step_function_node);
  main
;;

let make_stale (node : _ Node.t) =
  let t = node.state in
  node.recomputed_at <- Stabilization_num.none;
  (* force the node to be stale *)
  if Node.needs_to_be_computed node && not (Node.is_in_recompute_heap node)
  then Recompute_heap.add t.recompute_heap node
;;

let advance_clock (clock : Clock.t) ~to_ =
  let t = Clock.incr_state clock in
  ensure_not_stabilizing t ~name:"advance_clock" ~allow_in_update_handler:true;
  if debug then invariant t;
  if Time_ns.( > ) to_ (now clock)
  then (
    set_var_while_not_stabilizing clock.now to_;
    Timing_wheel.advance_clock clock.timing_wheel ~to_ ~handle_fired:clock.handle_fired;
    Timing_wheel.fire_past_alarms clock.timing_wheel ~handle_fired:clock.handle_fired;
    while Uopt.is_some clock.fired_alarm_values do
      let alarm_value = Uopt.unsafe_value clock.fired_alarm_values in
      clock.fired_alarm_values <- alarm_value.next_fired;
      alarm_value.next_fired <- Uopt.none;
      match alarm_value.action with
      | At { main; _ } ->
        if Node.is_valid main
        then (
          Node.set_kind main (Const After);
          make_stale main)
      | At_intervals ({ main; base; interval; _ } as at_intervals) ->
        if Node.is_valid main
        then (
          at_intervals.alarm
          <- add_alarm
               clock
               ~at:(next_interval_alarm_strict clock ~base ~interval)
               alarm_value;
          make_stale main)
      | Snapshot { main; value_at; _ } ->
        if debug then assert (Node.is_valid main);
        set_freeze main ~child:value_at ~only_freeze_when:(fun _ -> true);
        make_stale main
      | Step_function { main; _ } -> if Node.is_valid main then make_stale main
    done;
    if debug then invariant t)
;;

let create_clock t ~timing_wheel_config ~start =
  let timing_wheel = Timing_wheel.create ~config:timing_wheel_config ~start in
  let rec clock : Clock.t =
    { now = create_var t start
    ; handle_fired
    ; fired_alarm_values = Uopt.none
    ; timing_wheel
    }
  and handle_fired alarm =
    let alarm_value = Timing_wheel.Alarm.value clock.timing_wheel alarm in
    alarm_value.next_fired <- clock.fired_alarm_values;
    clock.fired_alarm_values <- Uopt.some alarm_value
  in
  clock
;;

let create (module Config : Config.Incremental_config) ~max_height_allowed =
  let adjust_heights_heap = Adjust_heights_heap.create ~max_height_allowed in
  let recompute_heap = Recompute_heap.create ~max_height_allowed in
  let t =
    { status = Not_stabilizing
    ; bind_lhs_change_should_invalidate_rhs = Config.bind_lhs_change_should_invalidate_rhs
    ; stabilization_num = Stabilization_num.zero
    ; current_scope = Scope.top
    ; adjust_heights_heap
    ; recompute_heap
    ; propagate_invalidity = Stack.create ()
    ; num_active_observers = 0
    ; all_observers = Uopt.none
    ; finalized_observers = Thread_safe_queue.create ()
    ; disallowed_observers = Stack.create ()
    ; new_observers = Stack.create ()
    ; set_during_stabilization = Stack.create ()
    ; handle_after_stabilization = Stack.create ()
    ; run_on_update_handlers = Stack.create ()
    ; only_in_debug = Only_in_debug.create ()
    ; weak_hashtbls = Thread_safe_queue.create ()
    ; keep_node_creation_backtrace = false
    ; num_nodes_became_necessary = 0
    ; num_nodes_became_unnecessary = 0
    ; num_nodes_changed = 0
    ; num_nodes_invalidated = 0
    ; num_nodes_created = 0
    ; num_nodes_recomputed = 0
    ; num_nodes_recomputed_directly_because_one_child = 0
    ; num_nodes_recomputed_directly_because_min_height = 0
    ; num_var_sets = 0
    }
  in
  t
;;

let weak_memoize_fun_by_key
      ?(initial_size = default_hash_table_initial_size)
      t
      hashable
      project_key
      f
  =
  let scope = t.current_scope in
  let table = Weak_hashtbl.create ~size:initial_size hashable in
  let packed = Packed_weak_hashtbl.T table in
  Weak_hashtbl.set_run_when_unused_data table ~thread_safe_f:(fun () ->
    Thread_safe_queue.enqueue t.weak_hashtbls packed);
  stage (fun a ->
    let key = project_key a in
    match Weak_hashtbl.find table key with
    | Some b -> b
    | None ->
      let b = within_scope t scope ~f:(fun () -> f a) in
      Weak_hashtbl.add_exn table ~key ~data:b;
      b)
;;

module Expert = struct
  (* Given that invalid node are at attempt at avoiding breaking the entire incremental
     computation on problems, let's just ignore any operation on an invalid incremental
     rather than raising. *)
  let expert_kind_of_node (node : _ Node.t) =
    match node.kind with
    | Expert e -> Uopt.some e
    | Invalid -> Uopt.none
    | kind -> raise_s [%sexp "unexpected kind for expert node", (kind : _ Kind.t)]
  ;;

  let create state ~on_observability_change f =
    let e = Expert.create ~f ~on_observability_change in
    let node = create_node state (Expert e) in
    if debug
    then
      if Option.is_some state.only_in_debug.currently_running_node
      then
        state.only_in_debug.expert_nodes_created_by_current_node
        <- T node :: state.only_in_debug.expert_nodes_created_by_current_node;
    node
  ;;

  let currently_running_node_exn state name =
    match state.only_in_debug.currently_running_node with
    | None -> raise_s [%sexp ("can only call " ^ name ^ " during stabilization" : string)]
    | Some current -> current
  ;;

  (* Note that the two following functions are not symmetric of one another: in [let y =
     map x], [x] is always a child of [y] (assuming [x] doesn't become invalid) but [y] in
     only a parent of [x] if y is necessary. *)
  let assert_currently_running_node_is_child state node name =
    let (T current) = currently_running_node_exn state name in
    if not (Node.has_child node ~child:current)
    then
      raise_s
        [%sexp
          ("can only call " ^ name ^ " on parent nodes" : string)
        , ~~(node.kind : _ Kind.t)
        , ~~(current.kind : _ Kind.t)]
  ;;

  let assert_currently_running_node_is_parent state node name =
    let (T current) = currently_running_node_exn state name in
    if not (Node.has_parent ~parent:current node)
    then
      raise_s
        [%sexp
          ("can only call " ^ name ^ " on children nodes" : string)
        , ~~(node.kind : _ Kind.t)
        , ~~(current.kind : _ Kind.t)]
  ;;

  let make_stale (node : _ Node.t) =
    let state = node.state in
    let e_opt = expert_kind_of_node node in
    if Uopt.is_some e_opt
    then (
      if debug then assert_currently_running_node_is_child state node "make_stale";
      let e = Uopt.unsafe_value e_opt in
      match Expert.make_stale e with
      | `Already_stale -> ()
      | `Ok ->
        if Node.is_necessary node && not (Node.is_in_recompute_heap node)
        then Recompute_heap.add state.recompute_heap node)
  ;;

  let invalidate (node : _ Node.t) =
    let state = node.state in
    if debug then assert_currently_running_node_is_child state node "invalidate";
    invalidate_node node;
    propagate_invalidity state
  ;;

  let add_dependency (node : _ Node.t) (dep : _ Expert.edge) =
    let state = node.state in
    let e_opt = expert_kind_of_node node in
    if Uopt.is_some e_opt
    then (
      if debug
      then
        if am_stabilizing state
        && not
             (List.mem
                ~equal:phys_equal
                state.only_in_debug.expert_nodes_created_by_current_node
                (T node))
        then assert_currently_running_node_is_child state node "add_dependency";
      let e = Uopt.unsafe_value e_opt in
      let new_child_index = Expert.add_child_edge e (E dep) in
      if Node.is_necessary node
      then (
        add_parent ~child:dep.child ~parent:node ~child_index:new_child_index;
        if debug then assert (Node.needs_to_be_computed node);
        if not (Node.is_in_recompute_heap node)
        then Recompute_heap.add state.recompute_heap node))
  ;;

  let remove_dependency (node : _ Node.t) (edge : _ Expert.edge) =
    let state = node.state in
    let e_opt = expert_kind_of_node node in
    if Uopt.is_some e_opt
    then (
      if debug then assert_currently_running_node_is_child state node "remove_dependency";
      let e = Uopt.unsafe_value e_opt in
      (* It would require additional thoughts to check whether allowing the node not to be
         necessary makes sense. *)
      assert (Node.is_necessary node);
      let edge_index = Uopt.value_exn edge.index in
      let (E last_edge) = Expert.last_child_edge_exn e in
      let last_edge_index = Uopt.value_exn last_edge.index in
      if edge_index <> last_edge_index
      then (
        Node.swap_children_except_in_kind
          node
          ~child1:edge.child
          ~child_index1:edge_index
          ~child2:last_edge.child
          ~child_index2:last_edge_index;
        Expert.swap_children e ~child_index1:edge_index ~child_index2:last_edge_index;
        if debug then Node.invariant ignore node);
      Expert.remove_last_child_edge_exn e;
      remove_child ~child:edge.child ~parent:node ~child_index:last_edge_index;
      if debug then assert (Node.needs_to_be_computed node);
      if not (Node.is_in_recompute_heap node)
      then Recompute_heap.add state.recompute_heap node;
      if not (Node.is_valid edge.child) then Expert.decr_invalid_children e)
  ;;
end
