open! Core.Std
open! Import

module Alarm = Timing_wheel.Alarm

type t = Types.Alarm_value.t sexp_opaque Alarm.t
with sexp_of

let invariant (_ : t) = ()

let null = Alarm.null ()
