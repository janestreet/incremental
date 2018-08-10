open! Core_kernel
open! Import

module Alarm = Timing_wheel_ns.Alarm

type t = Types.Alarm_value.t sexp_opaque Alarm.t
[@@deriving sexp_of]

let invariant (_ : t) = ()

let null = Alarm.null ()
