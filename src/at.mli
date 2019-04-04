(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An [At] is a kind of DAG node whose value is [Before] until the clock reaches a
    certain time, at which point its value becomes [After]. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.At
end

include Invariant.S with type t := t
include Sexp_of.S with type t := t
