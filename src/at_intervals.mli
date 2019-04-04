(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An [At_intervals.t] is a kind of DAG node that changes at a regular time interval. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.At_intervals
end

include Invariant.S with type t := t
include Sexp_of.S with type t := t
