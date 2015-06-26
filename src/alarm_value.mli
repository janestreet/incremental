(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An alarm value is stored in the timing wheel and is used to implement time-based
    functions: [at], [at_interval], [snapshot], [step_function].
*)

open! Core.Std
open! Import

include module type of struct include Types.Alarm_value end

include Invariant.S with type t := t
include Sexp_of.S   with type t := t

val create : Action.t -> t
