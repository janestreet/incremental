(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An ['a Step_function.t] is a kind of DAG node that represents a function from
    [Time_ns.t] to ['a] with a finite number of steps.  The steps are in nondecreasing
    time order. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Step_function_node
end

include Invariant.S1 with type 'a t := 'a t
include Sexp_of.S1 with type 'a t := 'a t

val advance : _ t -> to_:Time_ns.t -> unit
