(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An ['a Join.t] is a type of DAG node. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Join
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t
