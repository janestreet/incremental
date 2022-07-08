open! Core

(* Any tests in this file are here because they should not be run in the copy of
   the test suite that runs incremental in debug mode. Each test should have a
   comment with the reason that it is in this file. *)

(* Debug-enabled incremental allocates a bunch of words during stabilization,
   so we only run this test for non-debug incremental. *)
let%expect_test "stabilization that propagates values through an existing graph should \
                 not allocate"
  =
  let module I = Incremental.Make () in
  let open I in
  let v' = Var.create 0 in
  let v = map (Var.watch v') ~f:(( + ) 1) in
  let w' = Var.create 0 in
  let w = map (Var.watch w') ~f:(( + ) 1) in
  let a = map v ~f:(( + ) 1) in
  let b = map w ~f:(( + ) 1) in
  let o = observe (map2 ~f:( + ) a b) in
  (* The first stabilization allocates, but the next two do not. The
     point is that stabilization shouldn't allocate anything except to create
     new nodes. This means that it is okay for stabilization to allocate new
     nodes when the rhs of a bind node changes, but if the shape of the graph
     has not changed, then it shouldn't allocate.

     I'm not exactly sure why we care about not allocating. Obviously
     it's a good thing to not allocate, but I don't know whether there is an
     intended use-case of incremental where allocation is unacceptable.
     Regardless, I've received CRs when modifying incremental to maintain the
     property of not allocating during stabilization, so I wrote this test to
     notify the programmer if they ever change the allocation behavior of
     stabilization, either on purpose or by accident. *)
  stabilize ();
  Var.set v' 4;
  Expect_test_helpers_core.require_no_allocation [%here] stabilize;
  Var.set v' 5;
  Var.set w' 5;
  Expect_test_helpers_core.require_no_allocation [%here] stabilize;
  Observer.disallow_future_use o
;;
