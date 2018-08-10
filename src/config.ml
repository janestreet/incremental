open Core_kernel
open Import

include Config_intf

module Default () = struct
  let bind_lhs_change_should_invalidate_rhs = true

  let start = Time_ns.now ()

  module Alarm_precision = Timing_wheel_ns.Alarm_precision

  let timing_wheel_config =
    let alarm_precision = Alarm_precision.about_one_millisecond in
    let level_bits = [ 14; 13; 5 ] in
    Timing_wheel_ns.Config.create
      ~alarm_precision
      ~level_bits:(Timing_wheel_ns.Level_bits.create_exn level_bits)
      ()
  ;;
end
