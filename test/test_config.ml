open! Core
open! Import

let%expect_test "default timing-wheel precision and level durations" =
  let module Config = Incremental.Config.Default () in
  let config = Config.timing_wheel_config in
  let durations = Timing_wheel_ns.Config.durations config in
  require [%here] (Time_ns.Span.( >= ) (List.last_exn durations) Time_ns.Span.day);
  print_s [%message
    ""
      ~alarm_precision:(Timing_wheel_ns.Config.alarm_precision config : Time_ns.Span.t)
      (durations : Time_ns.Span.t list)];
  [%expect {|
    ((alarm_precision 1.048576ms)
     (durations (17.179869184s 1d15h5m37.488355328s 52d2h59m59.627370496s))) |}];
;;
