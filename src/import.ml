open Core
include Int.Replace_polymorphic_compare

let debug   = Incremental_kernel.Private.Import.debug
let verbose = Incremental_kernel.Private.Import.verbose

module Alarm_precision = Timing_wheel_ns.Alarm_precision
