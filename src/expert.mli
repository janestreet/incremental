(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An [Expert.t] is the only kind of node that can update its value and set of children
    incrementally. The operations to change the set of children and to react to various
    events (new value in a child etc) are exposed to the user. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Expert
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t

val sexp_of_edge : ('a -> Sexp.t) -> 'a edge -> Sexp.t
val invariant_about_num_invalid_children : _ t -> is_necessary:bool -> unit

val create
  :  f:(unit -> 'a)
  -> on_observability_change:(is_now_observable:bool -> unit)
  -> 'a t

val make_stale : _ t -> [ `Already_stale | `Ok ]
val incr_invalid_children : _ t -> unit
val decr_invalid_children : _ t -> unit

(** Returns the index of this new edge. *)
val add_child_edge : _ t -> packed_edge -> int

val swap_children : _ t -> child_index1:int -> child_index2:int -> unit
val last_child_edge_exn : _ t -> packed_edge
val remove_last_child_edge_exn : _ t -> unit
val before_main_computation : _ t -> [ `Invalid | `Ok ]
val observability_change : _ t -> is_now_observable:bool -> unit
val run_edge_callback : _ t -> child_index:int -> unit
