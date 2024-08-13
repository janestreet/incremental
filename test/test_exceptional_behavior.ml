open! Core
open! Import
module I = Incremental.Make ()
open I

exception My_exn [@@deriving sexp]

let my_function_that_fails () =
  let r = raise My_exn in
  print_endline "putting this here to keep the function from being TCO'd";
  r
;;

let require_does_raise here f =
  (* capture the exception with [show_backtrace] set to [true] *)
  Expect_test_helpers_base.require_does_raise
    ~show_backtrace:true
    ~hide_positions:false
    ~here
    f;
  (* check it to see that our function is in the backtrace, but don't print *)
  let with_backtrace = Expect_test_helpers_base.expect_test_output () in
  if not (String.is_substring with_backtrace ~substring:"my_function_that_fails")
  then print_endline with_backtrace;
  (* re-run the function, this time just printing the message *)
  Expect_test_helpers_base.require_does_raise
    ~show_backtrace:false
    ~hide_positions:true
    ~here
    f
;;

let%expect_test "exception handling / re-throwing" =
  let open I.Let_syntax in
  let x = Var.create () in
  let (y : Nothing.t I.t) =
    let%map () = Var.watch x in
    my_function_that_fails ()
  in
  let o = observe y in
  require_does_raise [%here] (fun () -> I.stabilize ());
  [%expect
    {|
    (exn.ml.Reraised
     "cannot stabilize -- stabilize previously raised"
     test_exceptional_behavior.ml.My_exn)
    |}];
  require_does_raise [%here] (fun () -> I.stabilize ());
  [%expect
    {|
    (exn.ml.Reraised
     "cannot stabilize -- stabilize previously raised"
     test_exceptional_behavior.ml.My_exn)
    |}];
  require_does_raise [%here] (fun () -> I.Observer.value_exn o);
  [%expect
    {|
    (exn.ml.Reraised
     "Observer.value_exn called after stabilize previously raised"
     test_exceptional_behavior.ml.My_exn)
    |}];
  require_does_raise [%here] (fun () -> I.Var.set x ());
  [%expect
    {|
    (exn.ml.Reraised
     "cannot set var -- stabilization previously raised"
     test_exceptional_behavior.ml.My_exn)
    |}]
;;
