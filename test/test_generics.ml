(* This code tests that generic functions can operate on [Incremental.Make] values. *)

open! Core_kernel
open! Import
module I = Incremental.Make ()

let%expect_test _ =
  let i = I.return () in
  let (_ : _) = Incremental.map i ~f:Fn.id in
  let o = I.observe i in
  let (_ : _) = Incremental.Observer.value o in
  let v = I.Var.create () in
  let (_ : _) = Incremental.Var.value v in
  let (_ : _) = Incremental.Scope.within I.State.t I.Scope.top ~f:Fn.id in
  let (_ : _) = Incremental.Clock.now (I.Clock.create ~start:Time_ns.epoch ()) in
  let (_ : _) =
    Incremental.Expert.Node.add_dependency
      (I.Expert.Node.create Fn.id)
      (I.Expert.Dependency.create i)
  in
  [%expect {||}]
;;

let%expect_test "[State.create]" =
  let module I = (val Incremental.State.create ()) in
  let state = I.t in
  let (_ : (int, I.state_witness) Incremental.t) = Incremental.const state 13 in
  ()
;;

let%expect_test "[sexp_of_state_witness]" =
  let module I = (val Incremental.State.create ()) in
  ignore ([%sexp (I.t : I.state_witness Incremental.State.t)] : Sexp.t)
;;
