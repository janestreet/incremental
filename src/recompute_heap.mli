(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    The recompute heap holds the set of nodes such that [Node.needs_to_be_computed].  It
    is used during stabilization to visit the nodes that need to be computed in
    topological order, using the recompute heap to visit them in increasing order of
    height. *)

open! Core_kernel
open! Import

type t = Types.Recompute_heap.t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : max_height_allowed:int -> t
val length : t -> int

(** [max_height_allowed] is the maximum [node.height] allowed for [node] in [t].

    It is an error to call [set_max_height_allowed t m] if there is a [node] in [t] with
    [node.height > m]. *)
val max_height_allowed : t -> int

val set_max_height_allowed : t -> int -> unit

(** [min_height t] returns the smallest height of any element in [t], or
    [max_height_allowed + 1] if [length t = 0]. *)
val min_height : t -> int

(** [add t node] should only be called iff:

    {[
      not (Node.is_in_recompute_heap node)
      && Node.needs_to_be_computed node
      && node.height <= max_height_allowed t
    ]} *)
val add : t -> _ Node.t -> unit

(** [remove t node] should only be called iff:

    {[
      Node.is_in_recompute_heap node
      && not (Node.needs_to_be_computed node)
    ]} *)
val remove : t -> _ Node.t -> unit

(** [remove_min t] removes and returns a node in [t] with minimum height.  [remove_min]
    should only be called if [length t > 0]. *)
val remove_min : t -> Node.Packed.t

(** [increase_height t node] should only be called when:

    - [node.height > node.height_in_recompute_heap]
    - [node.height <= max_height_allowed t]
    - [Node.is_in_recompute_heap node]

    It changes [node.height_in_recompute_heap] to equal [node.height] and adjusts [node]'s
    position in [t]. *)
val increase_height : t -> _ Node.t -> unit
