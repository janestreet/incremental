(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An ['a Cutoff.t] is a function that returns [true] if propagation of changes should be
    cutoff at a node based on the old value and the (possible) new value of the node. *)

open! Core_kernel
open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : (old_value:'a -> new_value:'a -> bool) -> 'a t
val of_compare : ('a -> 'a -> int) -> 'a t
val of_equal : ('a -> 'a -> bool) -> 'a t
val always : _ t
val never : _ t
val phys_equal : _ t
val poly_equal : _ t
val equal : 'a t -> 'a t -> bool
val should_cutoff : 'a t -> old_value:'a -> new_value:'a -> bool
