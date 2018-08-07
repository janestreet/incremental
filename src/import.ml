open Core_kernel
include Int.Replace_polymorphic_compare

let debug   = Incremental_kernel.Private.debug
let verbose = Incremental_kernel.Private.verbose

module Alarm_precision = Timing_wheel_ns.Alarm_precision
