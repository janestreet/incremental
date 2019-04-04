(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An ['a If_then_else.t] is a kind of DAG node. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.If_then_else
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t
