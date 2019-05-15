(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An [('a, 'acc) Unordered_array_fold.t] is a kind of DAG node, where ['a] is the type
    of value being folded and ['acc] is the type of the accumulator. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Unordered_array_fold
end

include Invariant.S2 with type ('a, 'acc) t := ('a, 'acc) t
include Sexp_of.S2 with type ('a, 'acc) t := ('a, 'acc) t

module Update : sig
  type ('a, 'b) t =
    | F_inverse of ('b -> 'a -> 'b)
    | Update of ('b -> old_value:'a -> new_value:'a -> 'b)
  [@@deriving sexp_of]
end

val create
  :  init:'acc
  -> f:('acc -> 'a -> 'acc)
  -> update:('a, 'acc) Update.t
  -> full_compute_every_n_changes:int
  -> children:'a Types.Node.t array
  -> main:'acc Types.Node.t
  -> ('a, 'acc) t

val compute : (_, 'acc) t -> 'acc

val child_changed
  :  ('a, _) t
  -> child:'b Types.Node.t
  -> child_index:int
  -> old_value_opt:'b Uopt.t
  -> new_value:'b
  -> unit

val force_full_compute : (_, _) t -> unit
