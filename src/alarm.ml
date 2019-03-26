open! Core_kernel
open! Import
module Alarm = Timing_wheel.Alarm

type t = (Types.Alarm_value.t[@sexp.opaque]) Alarm.t [@@deriving sexp_of]

let invariant (_ : t) = ()
let null = Alarm.null ()
