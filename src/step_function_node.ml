open Core_kernel
open! Import
open Types.Kind
module Alarm_value = Types.Alarm_value
module Node = Types.Node

type 'a t = 'a Types.Step_function_node.t =
  { main : 'a Node.t
  ; mutable child : 'a Step_function.t Node.t Uopt.t
  ; mutable extracted_step_function_from_child_at : Stabilization_num.t
  ; mutable value : 'a Uopt.t
  ; mutable upcoming_steps : (Time_ns.t * 'a) Sequence.t
  ; mutable alarm : Alarm.t
  ; mutable alarm_value : (Alarm_value.t[@sexp.opaque])
  ; clock : (Types.Clock.t[@sexp.opaque])
  }
[@@deriving fields, sexp_of]

let phys_same (t1 : _ t) (t2 : _ t) = phys_same t1 t2

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:
        (check (fun (main : _ Node.t) ->
           match main.kind with
           | Invalid -> ()
           | Const _ -> () (* happens when [upcoming_steps] becomes empty. *)
           | Step_function t' -> assert (phys_equal t t')
           | _ -> assert false))
      ~child:ignore
      ~extracted_step_function_from_child_at:ignore
      ~value:(check (Uopt.invariant invariant_a))
      ~upcoming_steps:ignore
      ~alarm:(check Alarm.invariant)
      ~alarm_value:
        (check (fun (alarm_value : Alarm_value.t) ->
           match alarm_value.action with
           | Step_function t2 -> assert (phys_same t t2)
           | _ -> assert false))
      ~clock:ignore)
;;

let rec advance_internal t ~to_ a1 steps =
  match Sequence.next steps with
  | Some ((step_at, a2), steps2) when Time_ns.( >= ) to_ step_at ->
    advance_internal t ~to_ a2 steps2
  | _ ->
    t.value <- Uopt.some a1;
    t.upcoming_steps <- steps
;;

let advance t ~to_ = advance_internal t ~to_ (Uopt.value_exn t.value) t.upcoming_steps
