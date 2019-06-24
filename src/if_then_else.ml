open Core_kernel
open! Import
open Types.Kind
module Node = Types.Node

type 'a t = 'a Types.If_then_else.t =
  { main : 'a Node.t
  ; test : bool Node.t
  ; test_change : unit Node.t
  ; mutable current_branch : 'a Node.t Uopt.t
  ; then_ : 'a Node.t
  ; else_ : 'a Node.t
  }
[@@deriving fields, sexp_of]

let same (t1 : _ t) (t2 : _ t) = phys_same t1 t2

let invariant _invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:
        (check (fun (main : _ Node.t) ->
           match main.kind with
           | Invalid -> ()
           | If_then_else t' -> assert (phys_equal t t')
           | _ -> ()))
      ~test:ignore
      ~test_change:
        (check (fun (test_change : _ Node.t) ->
           match test_change.kind with
           | Invalid -> ()
           | If_test_change t' -> assert (same t t')
           | _ -> assert false))
      ~current_branch:
        (check (fun current_branch ->
           if Uopt.is_some current_branch
           then (
             let current_branch = Uopt.value_exn current_branch in
             assert (
               phys_equal current_branch t.then_ || phys_equal current_branch t.else_
             ))))
      ~then_:ignore
      ~else_:ignore)
;;
