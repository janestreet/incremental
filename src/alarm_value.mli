(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An alarm value is stored in the timing wheel and is used to implement time-based
    functions: [at], [at_interval], [snapshot], [step_function]. *)

open! Core_kernel
open! Import

module Action : sig
  type t = Types.Alarm_value.Action.t
end

type t = Types.Alarm_value.t

include Invariant.S with type t := t
include Sexp_of.S with type t := t

val create : Action.t -> t
