open Core.Std
open Import    let _ = _squelch_unused_module_warning_
open Types.Kind

module Node = Types.Node

type 'a t = 'a Types.Snapshot.t =
  { main     : 'a Node.t
  ; at       : Time.t
  ; before   : 'a
  ; value_at : 'a Node.t
  }
with fields, sexp_of

let invariant invariant_a t =
  Invariant.invariant _here_ t <:sexp_of< _ t >> (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:(check (fun (main : _ Node.t) ->
        assert (Scope.is_top main.created_in);
        match main.kind with
        | Invalid -> () (* happens when snapshotting an invalid node *)
        | Const _ -> () (* happens after the snapshot *)
        | Snapshot t' -> assert (phys_equal t t')
        | _ -> assert false))
      ~at:ignore
      ~before:(check invariant_a)
      ~value_at:ignore)
;;
