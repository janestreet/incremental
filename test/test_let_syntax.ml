open! Core
open! Import

module I = Incremental.Make ()
open I

let%expect_test "simple examples of [let%map] and [let%bind]" =
  let () =
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
      if b
      then return 17 else return 19
    in
    let o1 = observe i1 in
    let o2 = observe i2 in
    I.stabilize ();
    print_s [%message
      ""
        (Observer.value o1 : int Or_error.t)
        (Observer.value o2 : int Or_error.t)];
  in
  [%expect {|
    (("Observer.value o1" (Ok 26))
     ("Observer.value o2" (Ok 17))) |}];
;;
