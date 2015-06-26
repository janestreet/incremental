open Core.Std
open Import    let _ = _squelch_unused_module_warning_
open Types.Kind

module Node = Types.Node

type t = Types.At.t =
  { main          : Before_or_after.t Node.t
  ; at            : Time.t
  ; mutable alarm : Alarm.t
  }
with fields, sexp_of

let invariant t =
  Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:(check (fun (main : Before_or_after.t Node.t) ->
        match main.kind with
        | Invalid -> ()
        | Const After -> () (* happens once the current time passes [t.at]. *)
        | At t' -> assert (phys_equal t t')
        | _ -> assert false))
      ~at:ignore
      ~alarm:(check Alarm.invariant))
;;
