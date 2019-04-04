(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An ['a Freeze.t] is a kind of DAG node that takes on the value of another node
    and doesn't change thereafter. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Freeze
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t
