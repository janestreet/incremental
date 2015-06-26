(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An ['a Step_function.t] is a kind of DAG node that represents a function from [Time.t]
    to ['a] with a finite number of steps.  The steps are in nondecreasing time order.
*)

open! Core.Std
open! Import

include module type of struct include Types.Step_function end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.  S1 with type 'a t := 'a t

val advance : _ t -> time_passed:(Time.t -> bool) -> unit
