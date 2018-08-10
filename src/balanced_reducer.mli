(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A [Balanced_reducer.t] is a mutable non-empty array that tracks the result of folding
    an associative operation ([reduce]) over the array as its elements change. *)

open! Core_kernel
open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** [create_exn ~len ~reduce] creates an array containing [len] [None]s and prepares an
    incremental fold with [reduce].  It raises if [len < 1]. *)
val create_exn
  :  ?sexp_of_a : ('a -> Sexp.t)  (** for improved error messages *)
  -> unit
  -> len : int
  -> reduce : ('a -> 'a -> 'a)
  -> 'a t

(** [set_exn t i a] updates the value at index [i] to [Some a].  It raises if [i] is out
    of bounds. *)
val set_exn : 'a t -> int -> 'a -> unit

(** [compute_exn t] computes the value of the fold.  It raises if any values of the array
    are [None]. *)
val compute_exn : 'a t -> 'a
