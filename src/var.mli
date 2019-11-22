(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A [Var.t] is a leaf in the incremental DAG. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Var
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t

module Packed : sig
  type t = Types.Var.Packed.t = T : _ Types.Var.t -> t [@@unboxed] [@@deriving sexp_of]
end

val latest_value : 'a t -> 'a
val incr_state : _ t -> Types.State.t
