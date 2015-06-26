(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A [Snapshot.t] is a kind of DAG node.
*)

open! Core.Std
open! Import

include module type of struct include Types.Snapshot end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.  S1 with type 'a t := 'a t
