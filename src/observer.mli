(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An observer is a "handle" to an {!Internal_observer} that is given to user code -- the
    handle exists so the implementation can hold on to the internal observer and use a
    finalizer to detect when the user is done with the observer.  The finalizer disallows
    future use of the observer if it has no on-update handlers, so even if user code uses
    a finalizer to resurrect the observer, it will still have [not (use_is_allowed t)]. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Observer
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t

val observing : 'a t -> 'a Node.t
val use_is_allowed : _ t -> bool
val value_exn : 'a t -> 'a
val on_update_exn : 'a t -> 'a On_update_handler.t -> unit
val incr_state : _ t -> Types.State.t
