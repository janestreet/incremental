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

module rec Alarm : sig
  type t = Alarm_value.t Timing_wheel_ns.Alarm.t
end = Alarm

and Alarm_value : sig
  module Action : sig
    type t =
      | At            of At.t
      | At_intervals  of At_intervals.t
      | Snapshot       : _ Snapshot.t -> t
      | Step_function  : _ Step_function.t -> t
  end
  type t =
    { action             : Action.t
    ; mutable next_fired : t Uopt.t
    }
end = Alarm_value

and Array_fold : sig
  type ('a, 'acc) t =
    { init     : 'acc
    ; f        : 'acc -> 'a -> 'acc
    ; children : 'a Node.t array
    }
end = Array_fold

and At : sig
  type t =
    { main          : Before_or_after.t Node.t
    ; at            : Time_ns.t
    ; mutable alarm : Alarm.t
    }
end = At

and At_intervals : sig
  type t =
    { main          : unit Node.t
    ; base          : Time_ns.t
    ; interval      : Time_ns.Span.t
    ; mutable alarm : Alarm.t
    }
end = At_intervals

and Bind : sig
  type ('a, 'b) t =
    { main                             : 'b Node.t
    ; mutable f                        : 'a -> 'b Node.t
    ; lhs                              : 'a Node.t
    ; lhs_change                       : unit Node.t
    ; mutable rhs                      : 'b Node.t Uopt.t
    ; mutable rhs_scope                : Scope.t
    ; mutable all_nodes_created_on_rhs : Packed_node.t Uopt.t
    }
end = Bind

and Expert : sig
  type 'a edge =
    { child         : 'a Node.t
    ; on_change     : 'a -> unit
    ; mutable index : int Uopt.t
    }

  type packed_edge = E : 'a edge -> packed_edge

  type 'a t =
    { f                               : unit -> 'a
    ; on_observability_change         : is_now_observable:bool -> unit
    ; mutable children                : packed_edge Uopt.t Array.t
    ; mutable num_children            : int
    ; mutable force_stale             : bool
    ; mutable num_invalid_children    : int
    ; mutable will_fire_all_callbacks : bool
    }
end = Expert

and Freeze : sig
  type 'a t =
    { main             : 'a Node.t
    ; child            : 'a Node.t
    ; only_freeze_when : ('a -> bool)
    }
end = Freeze

and If_then_else : sig
  type 'a t =
    { main                   : 'a Node.t
    ; test                   : bool Node.t
    ; test_change            : unit Node.t
    ; mutable current_branch : 'a Node.t Uopt.t
    ; then_                  : 'a Node.t
    ; else_                  : 'a Node.t
    }
end = If_then_else

and Internal_observer : sig
  module State : sig
    type t = Created | In_use | Disallowed | Unlinked
  end
  type 'a t =
    { mutable state              : State.t
    ; observing                  : 'a Node.t
    ; mutable on_update_handlers : 'a On_update_handler.t list
    ; mutable prev_in_all        : Packed_internal_observer.t Uopt.t
    ; mutable next_in_all        : Packed_internal_observer.t Uopt.t
    ; mutable prev_in_observing  : 'a t Uopt.t
    ; mutable next_in_observing  : 'a t Uopt.t
    }
end = Internal_observer

and Join : sig
  type 'a t =
    { main        : 'a Node.t
    ; lhs         : 'a Node.t Node.t
    ; lhs_change  : unit Node.t
    ; mutable rhs : 'a Node.t Uopt.t
    }
end = Join

and Kind : sig
  type 'a t =
    | Array_fold             : (_, 'a) Array_fold.t -> 'a t
    | At                     : At.t -> Before_or_after.t t
    | At_intervals           : At_intervals.t -> unit t
    | Bind_lhs_change        : (_, _) Bind.t -> unit t
    | Bind_main              : (_, 'a) Bind.t -> 'a t
    | Const                  of 'a
    | Expert                 of 'a Expert.t
    | Freeze                 of 'a Freeze.t
    | If_test_change         : _ If_then_else.t -> unit t
    | If_then_else           of 'a If_then_else.t
    | Invalid
    | Join_lhs_change        : _ Join.t -> unit t
    | Join_main              of 'a Join.t
    | Map                    : ('a1 -> 'a) * 'a1 Node.t -> 'a t
    | Snapshot               of 'a Snapshot.t
    | Step_function          of 'a Step_function.t
    | Uninitialized
    | Unordered_array_fold   : (_, 'a) Unordered_array_fold.t -> 'a t
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
end = Kind

and Node : sig
  type 'a t =
    { id                                        : Node_id.t
    ; mutable recomputed_at                     : Stabilization_num.t
    ; mutable value_opt                         : 'a Uopt.t
    ; mutable kind                              : 'a Kind.t
    ; mutable cutoff                            : 'a Cutoff.t
    ; mutable changed_at                        : Stabilization_num.t
    ; mutable num_on_update_handlers            : int
    ; mutable num_parents                       : int
    ; mutable parent1_and_beyond                : Packed_node.t Uopt.t array
    ; mutable parent0                           : Packed_node.t Uopt.t
    ; mutable created_in                        : Scope.t
    ; mutable next_node_in_same_scope           : Packed_node.t Uopt.t
    ; mutable height                            : int
    ; mutable height_in_recompute_heap          : int
    ; mutable prev_in_recompute_heap            : Packed_node.t Uopt.t
    ; mutable next_in_recompute_heap            : Packed_node.t Uopt.t
    ; mutable height_in_adjust_heights_heap     : int
    ; mutable next_in_adjust_heights_heap       : Packed_node.t Uopt.t
    ; mutable old_value_opt                     : 'a Uopt.t
    ; mutable observers                         : 'a Internal_observer.t Uopt.t
    ; mutable is_in_handle_after_stabilization  : bool
    ; mutable on_update_handlers                : 'a On_update_handler.t list
    ; mutable my_parent_index_in_child_at_index : int array
    ; mutable my_child_index_in_parent_at_index : int array
    ; mutable force_necessary                   : bool
    ; mutable user_info                         : Info.t option
    ; creation_backtrace                        : Backtrace.t option
    }
  [@@deriving sexp_of]

  val pack : _ t -> Packed_node.t
  val is_valid : _ t -> bool
  val is_necessary : _ t -> bool

end = struct
  include Node

  let sexp_of_t _ t = concat [ "n"; Node_id.to_string t.id ] |> [%sexp_of: string]

  let pack (type a) t = (Obj.magic (t : a t) : Should_not_use.t t)

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
    || (match t.kind with Freeze _ -> true | _ -> false)
    || t.force_necessary
  ;;
end

and Observer : sig
  type 'a t = 'a Internal_observer.t ref
end = Observer

and Packed_internal_observer : sig
  type t = T : _ Internal_observer.t -> t
end = Packed_internal_observer

and Packed_node : sig
  type t = Should_not_use.t Node.t [@@deriving sexp_of]
end = struct
  include Packed_node
  let sexp_of_t t = t |> [%sexp_of: _ Node.t]
end

and Scope : sig
  type t = Top | Bind : (_, _) Bind.t -> t [@@deriving sexp_of]
end = struct
  type t = Top | Bind : (_, _) Bind.t -> t

  let sexp_of_t = function
    | Top -> "Top" |> [%sexp_of: string]
    | Bind bind -> bind.main |> [%sexp_of: _ Node.t]
  ;;
end

and Snapshot : sig
  type 'a t =
    { main     : 'a Node.t
    ; at       : Time_ns.t
    ; before   : 'a
    ; value_at : 'a Node.t
    }
end = Snapshot

and Step_function : sig
  type 'a t =
    { main                   : 'a Node.t
    ; mutable value          : 'a
    ; mutable upcoming_steps : (Time_ns.t * 'a) list
    ; mutable alarm          : Alarm.t
    }
end = Step_function

and Unordered_array_fold : sig
  type ('a, 'acc) t =
    { main                                        : 'acc Node.t
    ; init                                        : 'acc
    ; f                                           : ('acc -> 'a -> 'acc)
    ; f_inverse                                   : ('acc -> 'a -> 'acc)
    ; full_compute_every_n_changes                : int
    ; children                                    : 'a Node.t array
    ; mutable fold_value                          : 'acc Uopt.t
    ; mutable num_changes_since_last_full_compute : int
    }
end = Unordered_array_fold

and Var : sig
  type 'a t =
    { mutable value                          : 'a
    ; mutable value_set_during_stabilization : 'a Uopt.t
    ; mutable set_at                         : Stabilization_num.t
    ; watch                                  : 'a Node.t
    }
end = Var
