(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A stabilization number is an int that corresponds to one round of stabilization (think
    of a round as running from the end of one stabilization to the end of the next
    stabilization).  Stabilization numbers are used to detect whether a node is stale,
    i.e. if one of its children changed at a stabilization since the node was
    recomputed. *)

open! Core_kernel
open! Import

type t = private int [@@deriving compare, sexp_of]

include Equal.S with type t := t
include Invariant.S with type t := t

(** [none <= t] for all [t]. *)
val none : t

val zero : t
val is_none : t -> bool
val is_some : t -> bool
val add1 : t -> t
val to_int : t -> int
