open! Core
open! Import
module I = Incremental.Make ()
open I

let%expect_test "simple examples of [let%map] and [let%bind]" =
  let open I.Let_syntax in
  let xi = Var.create 13 in
  let i1 =
    let%map_open a = watch xi
    and b = watch xi in
    a + b
  in
  let xb = Var.create true in
  let i2 =
    let%bind_open b = watch xb in
    if b then return 17 else return 19
  in
  let o1 = observe i1 in
  let o2 = observe i2 in
  I.stabilize ();
  print_s
    [%message
      "" (Observer.value o1 : int Or_error.t) (Observer.value o2 : int Or_error.t)];
  [%expect {|
    (("Observer.value o1" (Ok 26))
     ("Observer.value o2" (Ok 17))) |}]
;;

let%expect_test "simple example of using map3 via [let%mapn]" =
  let open I.Let_syntax in
  let x = Var.create 13 in
  let y = Var.create 42 in
  let z = Var.create 12 in
  let xyz =
    let%mapn x = Var.watch x
    and y = Var.watch y
    and z = Var.watch z in
    x, y, z
  in
  let o = observe xyz in
  I.stabilize ();
  print_s [%message (Observer.value o : (int * int * int) Or_error.t)];
  [%expect {| ("Observer.value o" (Ok (13 42 12))) |}];
  Var.set x 100;
  I.stabilize ();
  print_s [%message (Observer.value o : (int * int * int) Or_error.t)];
  [%expect {| ("Observer.value o" (Ok (100 42 12))) |}]
;;

let%expect_test "simple example of using bind3 via [let%bindn]" =
  let open I.Let_syntax in
  let x = Var.create 13 in
  let y = Var.create 42 in
  let z = Var.create 12 in
  let xyz =
    let%bindn x = Var.watch x
    and y = Var.watch y
    and z = Var.watch z in
    return (x, y, z)
  in
  let o = observe xyz in
  I.stabilize ();
  print_s [%message (Observer.value o : (int * int * int) Or_error.t)];
  [%expect {| ("Observer.value o" (Ok (13 42 12))) |}];
  Var.set x 100;
  I.stabilize ();
  print_s [%message (Observer.value o : (int * int * int) Or_error.t)];
  [%expect {| ("Observer.value o" (Ok (100 42 12))) |}]
;;
