(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An [('a, 'acc) Unordered_array_fold.t] is a kind of DAG node, where ['a] is the type
    of value being folded and ['acc] is the type of the accumulator.
*)

open! Core_kernel
open! Import

include module type of struct include Types.Unordered_array_fold end

include Invariant.S2 with type ('a, 'acc) t := ('a, 'acc) t
include Sexp_of.  S2 with type ('a, 'acc) t := ('a, 'acc) t

val create
  :  init:'acc
  -> f:('acc -> 'a -> 'acc)
  -> f_inverse:('acc -> 'a -> 'acc)
  -> full_compute_every_n_changes:int
  -> children:'a Types.Node.t array
  -> main:'acc Types.Node.t
  -> ('a, 'acc) t

val compute : (_, 'acc) t -> 'acc

val child_changed : ('a, _) t -> old_value_opt:'a Uopt.t -> new_value:'a -> unit

val force_full_compute : (_, _) t -> unit

