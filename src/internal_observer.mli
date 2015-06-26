(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An observer is a root of the incremental DAG -- all descendants of an observer are
    "necessary", so that stabilization ensures their values are up to date.
*)

open! Core.Std
open! Import

include module type of struct include Types.Internal_observer end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.  S1 with type 'a t := 'a t

val same : _ t -> _ t -> bool

val observing : 'a t -> 'a Node.t

val use_is_allowed : _ t -> bool

val value_exn : 'a t -> 'a

val on_update_exn : 'a t -> 'a On_update_handler.t -> unit

val unlink : _ t -> unit

module Packed : sig
  type t = Types.Packed_internal_observer.t = T : _ Types.Internal_observer.t -> t
  with sexp_of

  include Invariant.S with type t := t

  val next_in_all     : t -> t Uopt.t
  val set_prev_in_all : t -> t Uopt.t -> unit
end
