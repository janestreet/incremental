(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    [Uopt] is short for "unboxed option".  ['a Uopt.t] is like ['a option], but doesn't
    box [some] values.  It must not be used in a nested way, i.e. as ['a Uopt.t Uopt.t].
    It must also not be used for [float Uopt.t array], since the representation of the
    array would vary depending on whether [none] or [some] is used to create the array.
    It should also not be used in a record that contains only [float]s and [float
    Uopt.t]s, because the compiler would treat that as a float-only record and would unbox
    the record fields.

    The type is exposed as [private 'a] so the compiler can infer that an ['a t] can't be
    a float in the right context, thus avoiding runtime checks in [_ Uopt.t array]s.
    Using [:>] is of course unsafe.  Because [Uopt.none] cannot be garbage collected,
    there is no problem if the compiler decides to skip write barriers (when ['a] is an
    immediate, which probably doesn't happen in incremental anyway).
*)

open! Core.Std
open! Import

type 'a t = private 'a with sexp_of

include Invariant.S1 with type 'a t := 'a t

val none : _ t
val some : 'a -> 'a t
val is_none : _ t -> bool
val is_some : _ t -> bool
val value_exn : 'a t -> 'a

(* It is safe to call [unsafe_value t] iff [is_some t]. *)
val unsafe_value : 'a t -> 'a
