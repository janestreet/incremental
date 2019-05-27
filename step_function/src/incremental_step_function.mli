(** An ['a Step_function.t] is a function from [Time_ns.t] to ['a]. *)

open! Core_kernel

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val init : 'a t -> 'a
val steps : 'a t -> (Time_ns.t * 'a) Sequence.t
val value : 'a t -> at:Time_ns.t -> 'a

(** [constant a] is the step function [t] with [value t ~at = a] for all [at]. *)
val constant : 'a -> 'a t

(** [create_exn ~init ~steps:[(t_1, v_1); ...; (t_n, vn)]] is the step function [t] with
    [value t ~at = init] for [at < t_1], [value t ~at = vi] for [t_i <= at < t_i+1].
    [create_exn] raises if the times aren't in nondecreasing order, i.e.  if for some [i <
    j], [ti > tj]. *)
val create_exn : init:'a -> steps:(Time_ns.t * 'a) list -> 'a t

val create_from_sequence : init:'a -> steps:(Time_ns.t * 'a) Sequence.t -> 'a t
