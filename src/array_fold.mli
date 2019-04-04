(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An [Array_fold.t] is a kind of DAG node.  It is an immutable value that holds the
    children of type ['a] and can [compute] the fold to produce a value of type ['b]. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Array_fold
end

include Sexp_of.S2 with type ('a, 'b) t := ('a, 'b) t
include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

val compute : (_, 'b) t -> 'b
