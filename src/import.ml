open Core.Std
include Int.Replace_polymorphic_compare

let debug   = Incremental_kernel.Import.debug
let verbose = Incremental_kernel.Import.verbose

module Alarm_precision = Timing_wheel_ns.Alarm_precision
