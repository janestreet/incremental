open! Core
open! Import
open! Incremental.Private.Balanced_reducer

type a = int list [@@deriving sexp_of]

let show_reduce = ref true

let reduce a1 a2 =
  if !show_reduce then print_s [%message "reduce" ~_:(a1 : a) ~_:(a2 : a)];
  a1 @ a2
;;

let invariant t = invariant ignore t

let create_exn ~len : a t =
  let t = create_exn () ~len ~reduce ~sexp_of_a in
  invariant t;
  t
;;

let set_exn t i v =
  set_exn t i [ v ];
  invariant t;
;;

let compute_exn t =
  print_s [%message "computed" ~_:(compute_exn t : a)];
  invariant t;
;;

let show t = print_s [%message "" ~_:(t : a t)]

let%expect_test "[create] with invalid length" =
  show_raise (fun () -> create_exn ~len:0);
  [%expect {|
    (raised ("non-positive number of leaves in balanced reducer" (num_leaves 0))) |}];
;;

let%expect_test "[set_exn] with invalid index" =
  let t = create_exn ~len:1 in
  show_raise (fun () -> set_exn t (-1) 13);
  [%expect {|
    (raised ("attempt to set negative index in balanced reducer" (index -1))) |}];
  show_raise (fun () -> set_exn t 1 13);
  [%expect {|
    (raised (
      "attempt to set out of bounds index in balanced reducer"
      (index  1)
      (length 1))) |}];
;;

let%expect_test "[sexp_of_t]" =
  let t = create_exn ~len:1 in
  show t;
  [%expect {|
    (()) |}];
  set_exn t 0 13;
  show t;
  [%expect {|
    (((13))) |}];
  let t = create_exn ~len:2 in
  show t;
  [%expect {|
    (()
     ()) |}];
  set_exn t 0 13;
  show t;
  [%expect {|
    (((13)) ()) |}];
  set_exn t 1 14;
  show t;
  [%expect {|
    (((13))
     ((14))) |}];
;;

let%expect_test "[compute_exn] with a [None]" =
  let t = create_exn ~len:1 in
  show_raise (fun () -> compute_exn t);
  [%expect {|
    (raised (
      "attempt to compute balanced reducer with unset elements"
      (balanced_reducer (())))) |}];
;;

let%expect_test "[compute_exn] with a [None]" =
  let t = create_exn ~len:2 in
  set_exn t 0 13;
  show_raise (fun () -> compute_exn t);
  [%expect {|
    (raised (
      "attempt to compute balanced reducer with unset elements"
      (balanced_reducer (((13)) ())))) |}];
;;

let%expect_test "[compute_exn]" =
  let t = create_exn ~len:1 in
  set_exn t 0 13;
  compute_exn t;
  [%expect {|
    (computed (13)) |}];
;;

let%expect_test "[compute_exn] caches [reduce]" =
  let t = create_exn ~len:2 in
  set_exn t 0 13;
  set_exn t 1 14;
  compute_exn t;
  [%expect {|
    (reduce
      (13)
      (14))
    (computed (13 14)) |}];
  compute_exn t;
  [%expect {|
    (computed (13 14)) |}];
;;

let%expect_test "[compute_exn] recomputes when input changes" =
  let t = create_exn ~len:2 in
  set_exn t 0 13;
  set_exn t 1 14;
  compute_exn t;
  [%expect {|
    (reduce
      (13)
      (14))
    (computed (13 14)) |}];
  set_exn t 1 15;
  compute_exn t;
  [%expect {|
    (reduce
      (13)
      (15))
    (computed (13 15)) |}];
;;

let%expect_test "[compute_exn] only recomputes what's necessary" =
  let t = create_exn ~len:3 in
  set_exn t 0 13;
  set_exn t 1 14;
  set_exn t 2 15;
  compute_exn t;
  [%expect {|
    (reduce
      (13)
      (14))
    (reduce (13 14) (15))
    (computed (13 14 15)) |}];
  set_exn t 2 16;
  compute_exn t;
  [%expect {|
    (reduce (13 14) (16))
    (computed (13 14 16)) |}];
;;

let%expect_test "[compute_exn] only recomputes what's necessary; larger example" =
  let t = create_exn ~len:10 in
  for i = 0 to 9 do set_exn t i (i+13) done;
  compute_exn t;
  [%expect {|
    (reduce
      (21)
      (22))
    (reduce
      (19)
      (20))
    (reduce
      (19 20)
      (21 22))
    (reduce
      (17)
      (18))
    (reduce
      (15)
      (16))
    (reduce
      (13)
      (14))
    (reduce
      (13 14)
      (15 16))
    (reduce (13 14 15 16) (17 18))
    (reduce (13 14 15 16 17 18) (19 20 21 22))
    (computed (13 14 15 16 17 18 19 20 21 22)) |}];
  set_exn t 9 23;
  compute_exn t;
  [%expect {|
    (reduce
      (21)
      (23))
    (reduce
      (19 20)
      (21 23))
    (reduce (13 14 15 16 17 18) (19 20 21 23))
    (computed (13 14 15 16 17 18 19 20 21 23)) |}];
  set_exn t 0 12;
  set_exn t 9 24;
  compute_exn t;
  [%expect {|
    (reduce
      (21)
      (24))
    (reduce
      (19 20)
      (21 24))
    (reduce
      (12)
      (14))
    (reduce
      (12 14)
      (15 16))
    (reduce (12 14 15 16) (17 18))
    (reduce (12 14 15 16 17 18) (19 20 21 24))
    (computed (12 14 15 16 17 18 19 20 21 24)) |}];
;;

let%expect_test "different lengths" =
  Ref.set_temporarily show_reduce false ~f:(fun () ->
    for len = 1 to 10 do
      let t = create_exn ~len in
      for i = 0 to len - 1 do
        set_exn t i i;
      done;
      compute_exn t;
      for i = 0 to len - 1 do
        set_exn t i i;
      done;
      for i = 0 to len - 1 do
        set_exn t i (len -1 - i);
      done;
      compute_exn t;
    done);
  [%expect {|
    (computed (0))
    (computed (0))
    (computed (0 1))
    (computed (1 0))
    (computed (0 1 2))
    (computed (2 1 0))
    (computed (0 1 2 3))
    (computed (3 2 1 0))
    (computed (0 1 2 3 4))
    (computed (4 3 2 1 0))
    (computed (0 1 2 3 4 5))
    (computed (5 4 3 2 1 0))
    (computed (0 1 2 3 4 5 6))
    (computed (6 5 4 3 2 1 0))
    (computed (0 1 2 3 4 5 6 7))
    (computed (7 6 5 4 3 2 1 0))
    (computed (0 1 2 3 4 5 6 7 8))
    (computed (8 7 6 5 4 3 2 1 0))
    (computed (0 1 2 3 4 5 6 7 8 9))
    (computed (9 8 7 6 5 4 3 2 1 0)) |}];
;;
