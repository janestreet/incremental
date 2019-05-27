open! Core_kernel
open! Expect_test_helpers_base
module Step_function = Incremental_step_function
open! Step_function

module T = struct
  type t = int Step_function.t [@@deriving sexp_of]
end

let show t = print_s [%sexp (t : int t)]
let time = Time_ns.of_int_ns_since_epoch
let value t ~at = print_s [%sexp (value t ~at:(time at) : int)]

let create_exn ~init ~steps =
  create_exn ~init ~steps:(List.map steps ~f:(fun (i, a) -> time i, a))
;;

let%expect_test "[constant]" =
  let t = constant 13 in
  invariant ignore t;
  show t;
  [%expect {|
    ((init 13) (steps ())) |}];
  value t ~at:0;
  [%expect {|
    13 |}]
;;

let%expect_test "empty [~steps] is same as constant" =
  show (create_exn ~init:13 ~steps:[]);
  [%expect {|
    ((init 13) (steps ())) |}]
;;

let%expect_test "[create_exn] raise" =
  require_does_raise [%here] ~hide_positions:true (fun () ->
    create_exn ~init:13 ~steps:[ 1, 14; 0, 15 ]);
  [%expect
    {|
    ("[Step_function.create_exn] got unsorted times"
     (steps ("1970-01-01 00:00:00.000000001Z" "1970-01-01 00:00:00Z"))) |}]
;;

let%expect_test "steps" =
  let t = create_exn ~init:13 ~steps:[ 1, 14; 1, 15; 2, 16 ] in
  invariant ignore t;
  show t;
  [%expect
    {|
    ((init 13)
     (steps (
       ("1970-01-01 00:00:00.000000001Z" 14)
       ("1970-01-01 00:00:00.000000001Z" 15)
       ("1970-01-01 00:00:00.000000002Z" 16)))) |}];
  value t ~at:0;
  [%expect {|
    13 |}];
  value t ~at:1;
  [%expect {|
    15 |}];
  value t ~at:2;
  [%expect {|
    16 |}];
  value t ~at:3;
  [%expect {|
    16 |}]
;;
