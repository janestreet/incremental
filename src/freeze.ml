open Core_kernel
open! Import
open Types.Kind
module Node = Types.Node

type 'a t = 'a Types.Freeze.t =
  { main : 'a Node.t
  ; child : 'a Node.t
  ; only_freeze_when : 'a -> bool
  }
[@@deriving fields, sexp_of]

let invariant _invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:
        (check (fun (main : _ Node.t) ->
           assert (Scope.is_top main.created_in);
           match main.kind with
           | Invalid -> () (* happens when freezing an invalid value *)
           | Const _ -> () (* happens on becoming frozen *)
           | Freeze t' -> assert (phys_equal t t')
           | _ -> assert false))
      ~child:ignore
      ~only_freeze_when:ignore)
;;
