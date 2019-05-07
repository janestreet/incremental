open! Core
open! Import

let%expect_test "default timing-wheel precision and level durations" =
  let module I = Incremental.Make () in
  let config = I.Clock.default_timing_wheel_config in
  let durations = Timing_wheel.Config.durations config in
  require [%here] (Time_ns.Span.( >= ) (List.last_exn durations) Time_ns.Span.day);
  print_s
    [%message
      ""
        ~alarm_precision:(Timing_wheel.Config.alarm_precision config : Time_ns.Span.t)
        (durations : Time_ns.Span.t list)];
  [%expect
    {|
    ((alarm_precision 1.048576ms)
     (durations (
       17.179869184s
       1d15h5m37.488355328s
       52d2h59m59.627370496s
       104d5h59m59.254740992s
       208d11h59m58.509481984s
       416d23h59m57.018963968s
       833d23h59m54.037927936s
       1667d23h59m48.075855872s
       3335d23h59m36.151711744s
       6671d23h59m12.303423488s
       13343d23h58m24.606846976s
       26687d23h56m49.213693952s
       53375d23h53m38.427387903s))) |}]
;;

let%expect_test "default timing wheel can handle the full range of times" =
  let module I = Incremental.Make () in
  let open I in
  let clock = Clock.create ~start:Time_ns.epoch () in
  let o = observe (Clock.at clock Time_ns.max_value_representable) in
  let show_o () = print_s [%sexp (o : Before_or_after.t Observer.t)] in
  stabilize ();
  show_o ();
  [%expect {|
    Before |}];
  Clock.advance_clock clock ~to_:Time_ns.max_value_representable;
  stabilize ();
  show_o ();
  [%expect {|
    After |}]
;;
