open Core_kernel
open! Import
open Types.Kind
module Node = Types.Node

type t = Types.At_intervals.t =
  { main : unit Node.t
  ; base : Time_ns.t
  ; interval : Time_ns.Span.t
  ; mutable alarm : Alarm.t
  ; clock : (Types.Clock.t[@sexp.opaque])
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:
        (check (fun (main : _ Node.t) ->
           match main.kind with
           | Invalid -> ()
           | At_intervals t' -> assert (phys_equal t t')
           | _ -> assert false))
      ~base:ignore
      ~interval:(check (fun interval -> assert (Time_ns.Span.is_positive interval)))
      ~alarm:(check Alarm.invariant)
      ~clock:ignore)
;;
