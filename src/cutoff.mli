@@ portable

(** A module internal to Incremental. Users should see {!Incremental_intf}.

    An ['a Cutoff.t] is a function that returns [true] if propagation of changes should be
    cutoff at a node based on the old value and the (possible) new value of the node. *)

open! Core
open! Import

type 'a t : value mod contended [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : (old_value:'a -> new_value:'a -> bool) -> 'a t
val of_compare : ('a -> 'a -> int) -> 'a t
val of_equal : ('a -> 'a -> bool) -> 'a t
val always : _ t
val never : _ t
val get_always : unit -> _ t
val get_never : unit -> _ t
val phys_equal : _ t
val poly_equal : _ t
val equal : 'a t -> 'a t -> bool
val should_cutoff : 'a t -> old_value:'a -> new_value:'a -> bool

module For_analyzer : sig
  type 'a t' := 'a t

  type t =
    | Always
    | Never
    | Phys_equal
    | Compare
    | Equal
    | F
  [@@deriving sexp, equal]

  val of_cutoff : _ t' -> t
  val to_string : t -> string
end
