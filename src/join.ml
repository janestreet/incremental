open Core_kernel
open! Import
open Types.Kind
module Node = Types.Node

type 'a t = 'a Types.Join.t =
  { main : 'a Node.t
  ; lhs : 'a Node.t Node.t
  ; lhs_change : unit Node.t
  ; mutable rhs : 'a Node.t Uopt.t
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
           | Join_main t' -> assert (same t t')
           | _ -> assert false))
      ~lhs:ignore
      ~lhs_change:
        (check (fun (lhs_change : _ Node.t) ->
           match lhs_change.kind with
           | Invalid -> ()
           | Join_lhs_change t' -> assert (same t t')
           | _ -> assert false))
      ~rhs:ignore)
;;
