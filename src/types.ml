(* This module has a giant [module rec] defining all the mutually recursive types used
   in the implementation.  The definition of each type is then repeated in its file;
   comments belong there, not here.

   We put just enough sexp converters here to display node ids.  The detailed sexp
   converters are generated via [with sexp] in the individual files.  Defining the sexp
   converters outside of the [module rec] makes it impossible to accidentally introduce a
   sexp converter that would try to produce an infinite sexp.
*)

open Core_kernel
open Import

module rec Adjust_heights_heap : sig
  type t =
    { mutable length : int
    ; mutable height_lower_bound : int
    ; mutable max_height_seen : int
    ; mutable nodes_by_height : Node.Packed.t Uopt.t array
    }
end =
  Adjust_heights_heap

and Alarm : sig
  type t = Alarm_value.t Timing_wheel.Alarm.t
end =
  Alarm

and Alarm_value : sig
  module Action : sig
    type t =
      | At of At.t
      | At_intervals of At_intervals.t
      | Snapshot : _ Snapshot.t -> t
      | Step_function : _ Step_function_node.t -> t
  end

  type t =
    { action : Action.t
    ; mutable next_fired : t Uopt.t
    }
end =
  Alarm_value

and Array_fold : sig
  type ('a, 'acc) t =
    { init : 'acc
    ; f : 'acc -> 'a -> 'acc
    ; children : 'a Node.t array
    }
end =
  Array_fold

and At : sig
  type t =
    { main : Before_or_after.t Node.t
    ; at : Time_ns.t
    ; mutable alarm : Alarm.t
    ; clock : Clock.t
    }
end =
  At

and At_intervals : sig
  type t =
    { main : unit Node.t
    ; base : Time_ns.t
    ; interval : Time_ns.Span.t
    ; mutable alarm : Alarm.t
    ; clock : Clock.t
    }
end =
  At_intervals

and Bind : sig
  type ('a, 'b) t =
    { main : 'b Node.t
    ; mutable f : 'a -> 'b Node.t
    ; lhs : 'a Node.t
    ; lhs_change : unit Node.t
    ; mutable rhs : 'b Node.t Uopt.t
    ; mutable rhs_scope : Scope.t
    ; mutable all_nodes_created_on_rhs : Node.Packed.t Uopt.t
    }
end =
  Bind

and Clock : sig
  type t =
    { timing_wheel : Alarm_value.t Timing_wheel.t
    ; now : Time_ns.t Var.t
    ; handle_fired : Alarm.t -> unit
    ; mutable fired_alarm_values : Alarm_value.t Uopt.t
    }
end =
  Clock

and Expert : sig
  type 'a edge =
    { child : 'a Node.t
    ; on_change : 'a -> unit
    ; mutable index : int Uopt.t
    }

  type packed_edge = E : 'a edge -> packed_edge [@@unboxed]

  type 'a t =
    { f : unit -> 'a
    ; on_observability_change : is_now_observable:bool -> unit
    ; mutable children : packed_edge Uopt.t Array.t
    ; mutable num_children : int
    ; mutable force_stale : bool
    ; mutable num_invalid_children : int
    ; mutable will_fire_all_callbacks : bool
    }
end =
  Expert

and Freeze : sig
  type 'a t =
    { main : 'a Node.t
    ; child : 'a Node.t
    ; only_freeze_when : 'a -> bool
    }
end =
  Freeze

and If_then_else : sig
  type 'a t =
    { main : 'a Node.t
    ; test : bool Node.t
    ; test_change : unit Node.t
    ; mutable current_branch : 'a Node.t Uopt.t
    ; then_ : 'a Node.t
    ; else_ : 'a Node.t
    }
end =
  If_then_else

and Internal_observer : sig
  module State : sig
    type t =
      | Created
      | In_use
      | Disallowed
      | Unlinked
  end

  type 'a t =
    { mutable state : State.t
    ; observing : 'a Node.t
    ; mutable on_update_handlers : 'a On_update_handler.t list
    ; mutable prev_in_all : Internal_observer.Packed.t Uopt.t
    ; mutable next_in_all : Internal_observer.Packed.t Uopt.t
    ; mutable prev_in_observing : 'a t Uopt.t
    ; mutable next_in_observing : 'a t Uopt.t
    }

  type 'a internal_observer = 'a t

  module Packed : sig
    type t = T : _ internal_observer -> t [@@unboxed]
  end
end =
  Internal_observer

and Join : sig
  type 'a t =
    { main : 'a Node.t
    ; lhs : 'a Node.t Node.t
    ; lhs_change : unit Node.t
    ; mutable rhs : 'a Node.t Uopt.t
    }
end =
  Join

and Kind : sig
  type 'a t =
    | Array_fold : (_, 'a) Array_fold.t -> 'a t
    | At : At.t -> Before_or_after.t t
    | At_intervals : At_intervals.t -> unit t
    | Bind_lhs_change : (_, _) Bind.t -> unit t
    | Bind_main : (_, 'a) Bind.t -> 'a t
    | Const of 'a
    | Expert of 'a Expert.t
    | Freeze of 'a Freeze.t
    | If_test_change : _ If_then_else.t -> unit t
    | If_then_else of 'a If_then_else.t
    | Invalid
    | Join_lhs_change : _ Join.t -> unit t
    | Join_main of 'a Join.t
    | Map : ('a1 -> 'a) * 'a1 Node.t -> 'a t
    | Snapshot of 'a Snapshot.t
    | Step_function of 'a Step_function_node.t
    | Uninitialized
    | Unordered_array_fold : (_, 'a) Unordered_array_fold.t -> 'a t
    | Var of 'a Var.t
    | Map2 : ('a1 -> 'a2 -> 'a) * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map3 : ('a1 -> 'a2 -> 'a3 -> 'a) * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
    | Map4 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        -> 'a t
    | Map5 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        -> 'a t
    | Map6 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        -> 'a t
    | Map7 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        -> 'a t
    | Map8 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        -> 'a t
    | Map9 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        -> 'a t
    | Map10 :
        ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a10 -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        * 'a10 Node.t
        -> 'a t
    | Map11 :
        ('a1
         -> 'a2
         -> 'a3
         -> 'a4
         -> 'a5
         -> 'a6
         -> 'a7
         -> 'a8
         -> 'a9
         -> 'a10
         -> 'a11
         -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        * 'a10 Node.t
        * 'a11 Node.t
        -> 'a t
    | Map12 :
        ('a1
         -> 'a2
         -> 'a3
         -> 'a4
         -> 'a5
         -> 'a6
         -> 'a7
         -> 'a8
         -> 'a9
         -> 'a10
         -> 'a11
         -> 'a12
         -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        * 'a10 Node.t
        * 'a11 Node.t
        * 'a12 Node.t
        -> 'a t
    | Map13 :
        ('a1
         -> 'a2
         -> 'a3
         -> 'a4
         -> 'a5
         -> 'a6
         -> 'a7
         -> 'a8
         -> 'a9
         -> 'a10
         -> 'a11
         -> 'a12
         -> 'a13
         -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        * 'a10 Node.t
        * 'a11 Node.t
        * 'a12 Node.t
        * 'a13 Node.t
        -> 'a t
    | Map14 :
        ('a1
         -> 'a2
         -> 'a3
         -> 'a4
         -> 'a5
         -> 'a6
         -> 'a7
         -> 'a8
         -> 'a9
         -> 'a10
         -> 'a11
         -> 'a12
         -> 'a13
         -> 'a14
         -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        * 'a10 Node.t
        * 'a11 Node.t
        * 'a12 Node.t
        * 'a13 Node.t
        * 'a14 Node.t
        -> 'a t
    | Map15 :
        ('a1
         -> 'a2
         -> 'a3
         -> 'a4
         -> 'a5
         -> 'a6
         -> 'a7
         -> 'a8
         -> 'a9
         -> 'a10
         -> 'a11
         -> 'a12
         -> 'a13
         -> 'a14
         -> 'a15
         -> 'a)
        * 'a1 Node.t
        * 'a2 Node.t
        * 'a3 Node.t
        * 'a4 Node.t
        * 'a5 Node.t
        * 'a6 Node.t
        * 'a7 Node.t
        * 'a8 Node.t
        * 'a9 Node.t
        * 'a10 Node.t
        * 'a11 Node.t
        * 'a12 Node.t
        * 'a13 Node.t
        * 'a14 Node.t
        * 'a15 Node.t
        -> 'a t
end =
  Kind

and Node : sig
  type 'a t =
    { id : Node_id.t
    ; state : State.t
    ; mutable recomputed_at : Stabilization_num.t
    ; mutable value_opt : 'a Uopt.t
    ; mutable kind : 'a Kind.t
    ; mutable cutoff : 'a Cutoff.t
    ; mutable changed_at : Stabilization_num.t
    ; mutable num_on_update_handlers : int
    ; mutable num_parents : int
    ;
      mutable parent1_and_beyond : Node.Packed.t Uopt.t array
    ; mutable parent0 : Node.Packed.t Uopt.t
    ; mutable created_in : Scope.t
    ; mutable next_node_in_same_scope : Node.Packed.t Uopt.t
    ; mutable height : int
    ; mutable height_in_recompute_heap : int
    ; mutable prev_in_recompute_heap : Node.Packed.t Uopt.t
    ; mutable next_in_recompute_heap : Node.Packed.t Uopt.t
    ; mutable height_in_adjust_heights_heap : int
    ; mutable next_in_adjust_heights_heap : Node.Packed.t Uopt.t
    ; mutable old_value_opt : 'a Uopt.t
    ; mutable observers : 'a Internal_observer.t Uopt.t
    ; mutable is_in_handle_after_stabilization : bool
    ; mutable on_update_handlers : 'a On_update_handler.t list
    ; mutable my_parent_index_in_child_at_index : int array
    ; mutable my_child_index_in_parent_at_index : int array
    ; mutable force_necessary : bool
    ; mutable user_info : Info.t option
    ; creation_backtrace : Backtrace.t option
    }
  [@@deriving sexp_of]

  module Packed : sig
    type 'a node = 'a t
    type t = T : _ node -> t [@@deriving sexp_of] [@@unboxed]
  end

  val is_valid : _ t -> bool
  val is_necessary : _ t -> bool
  val type_equal_if_phys_same : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
end = struct
  include (
    Node :
      module type of struct
      include Node
    end
    with module Packed := Node.Packed)

  let sexp_of_t _ t = concat [ "n"; Node_id.to_string t.id ] |> [%sexp_of: string]

  module Packed = struct
    type 'a node = 'a t [@@deriving sexp_of]
    type t = T : _ node -> t [@@unboxed]

    let sexp_of_t (T t) = t |> [%sexp_of: _ node]
  end

  let is_valid t =
    match t.kind with
    | Invalid -> false
    | _ -> true
  ;;

  (* [is_necessary] is defined here because we need it before node.ml is available.  It is
     used during graph manipulation, and so is written with some care to be fast. *)
  let is_necessary t =
    t.num_parents > 0
    || Uopt.is_some t.observers
    || (match t.kind with
      | Freeze _ -> true
      | _ -> false)
    || t.force_necessary
  ;;

  let t_is_injective (type a b) (T : (a t, b t) Type_equal.t) : (a, b) Type_equal.t = T

  let type_equal_if_phys_same (type a b) (t1 : a t) (t2 : b t) =
    (* This is type-safe assuming no one can give the same incremental node two different
       types.  This is true because the field [mutable old_value_opt : 'a option] prevents
       both subtyping and parameteric polymorphism.  But this allows to break
       abstractions, as in someone could write:

       {[
         type t
         type u (* = t underneath *)
         val create : unit -> t Incr.t * u Incr.t (* the two incrementals are phys_equal *)
       ]}

       and we would figure out that type t = u.  However, we could add a Type_equal.Id to
       nodes and do the same, so it seems to be a more general issue. *)
    if phys_same t1 t2
    then Some (t_is_injective (Obj.magic (T : _ Type_equal.t) : (a t, b t) Type_equal.t))
    else None
  ;;
end

and Observer : sig
  type 'a t = 'a Internal_observer.t ref
end =
  Observer

and Only_in_debug : sig
  type t =
    { mutable currently_running_node : Node.Packed.t option
    ; mutable expert_nodes_created_by_current_node : Node.Packed.t list
    }
end =
  Only_in_debug

and Packed_weak_hashtbl : sig
  type t = T : (_, _) Weak_hashtbl.t -> t
end =
  Packed_weak_hashtbl

and Recompute_heap : sig
  type t =
    { mutable length : int
    ; mutable height_lower_bound : int
    ; mutable nodes_by_height : Node.Packed.t Uopt.t array
    }
end =
  Recompute_heap

and Run_on_update_handlers : sig
  type t = T : 'a Node.t * 'a On_update_handler.Node_update.t -> t
end =
  Run_on_update_handlers

and Scope : sig
  type t =
    | Top
    | Bind : (_, _) Bind.t -> t
  [@@deriving sexp_of]
end = struct
  type t =
    | Top
    | Bind : (_, _) Bind.t -> t

  let sexp_of_t = function
    | Top -> "Top" |> [%sexp_of: string]
    | Bind bind -> bind.main |> [%sexp_of: _ Node.t]
  ;;
end

and Snapshot : sig
  type 'a t =
    { main : 'a Node.t
    ; at : Time_ns.t
    ; before : 'a
    ; value_at : 'a Node.t
    ; clock : Clock.t
    }
end =
  Snapshot

and State : sig
  type t =
    { mutable status : Status.t
    ; bind_lhs_change_should_invalidate_rhs : bool
    ; mutable stabilization_num : Stabilization_num.t
    ; mutable current_scope : Scope.t
    ; recompute_heap : Recompute_heap.t
    ; adjust_heights_heap : Adjust_heights_heap.t
    ; propagate_invalidity : Node.Packed.t Stack.t
    ; mutable num_active_observers : int
    ; mutable all_observers : Internal_observer.Packed.t Uopt.t
    ; finalized_observers : Internal_observer.Packed.t Thread_safe_queue.t
    ; new_observers : Internal_observer.Packed.t Stack.t
    ; disallowed_observers : Internal_observer.Packed.t Stack.t
    ; set_during_stabilization : Var.Packed.t Stack.t
    ; handle_after_stabilization : Node.Packed.t Stack.t
    ; run_on_update_handlers : Run_on_update_handlers.t Stack.t
    ; mutable only_in_debug : Only_in_debug.t
    ; weak_hashtbls : Packed_weak_hashtbl.t Thread_safe_queue.t
    ; mutable keep_node_creation_backtrace : bool
    ; mutable num_nodes_became_necessary : int
    ; mutable num_nodes_became_unnecessary : int
    ; mutable num_nodes_changed : int
    ; mutable num_nodes_created : int
    ; mutable num_nodes_invalidated : int
    ; mutable num_nodes_recomputed : int
    ; mutable num_nodes_recomputed_directly_because_one_child : int
    ; mutable num_nodes_recomputed_directly_because_min_height : int
    ; mutable num_var_sets : int
    }
end =
  State

and Status : sig
  type t =
    | Stabilizing
    | Running_on_update_handlers
    | Not_stabilizing
    | Stabilize_previously_raised of Raised_exn.t
end =
  Status

and Step_function_node : sig
  type 'a t =
    { main : 'a Node.t
    ; mutable child : 'a Step_function.t Node.t Uopt.t
    ; mutable extracted_step_function_from_child_at : Stabilization_num.t
    ; mutable value : 'a Uopt.t
    ; mutable upcoming_steps : (Time_ns.t * 'a) Sequence.t
    ; mutable alarm : Alarm.t
    ; mutable alarm_value : Alarm_value.t
    ; clock : Clock.t
    }
end =
  Step_function_node

and Unordered_array_fold : sig
  type ('a, 'acc) t =
    { main : 'acc Node.t
    ; init : 'acc
    ; f : 'acc -> 'a -> 'acc
    ; update : 'acc -> old_value:'a -> new_value:'a -> 'acc
    ; full_compute_every_n_changes : int
    ; children : 'a Node.t array
    ; mutable fold_value : 'acc Uopt.t
    ; mutable num_changes_since_last_full_compute : int
    }
end =
  Unordered_array_fold

and Var : sig
  type 'a t =
    { mutable value : 'a
    ; mutable value_set_during_stabilization : 'a Uopt.t
    ; mutable set_at : Stabilization_num.t
    ; watch : 'a Node.t
    }

  type 'a var := 'a t

  module Packed : sig
    type t = T : _ var -> t [@@unboxed]
  end
end =
  Var
