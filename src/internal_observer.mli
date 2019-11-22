(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An observer is a root of the incremental DAG -- all descendants of an observer are
    "necessary", so that stabilization ensures their values are up to date. *)

open! Core_kernel
open! Import

type 'a t = 'a Types.Internal_observer.t

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t

val same : _ t -> _ t -> bool
val observing : 'a t -> 'a Node.t
val use_is_allowed : _ t -> bool
val value_exn : 'a t -> 'a
val on_update_exn : 'a t -> 'a On_update_handler.t -> unit
val unlink : _ t -> unit
val incr_state : _ t -> Types.State.t

module Packed : sig
  type t = Types.Internal_observer.Packed.t = T : _ Types.Internal_observer.t -> t
  [@@unboxed] [@@deriving sexp_of]

  include Invariant.S with type t := t

  val next_in_all : t -> t Uopt.t
  val set_prev_in_all : t -> t Uopt.t -> unit
end
