open Core_kernel
open! Import
open Types.Kind

module Node = Types.Node

type 'a t = 'a Types.Step_function.t =
  { main                   : 'a Node.t
  ; mutable value          : 'a
  ; mutable upcoming_steps : (Time_ns.t * 'a) list
  ; mutable alarm          : Alarm.t
  }
[@@deriving fields, sexp_of]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:(check (fun (main : _ Node.t) ->
        match main.kind with
        | Invalid -> ()
        | Const _ -> () (* happens when [upcoming_steps] becomes empty. *)
        | Step_function t' -> assert (phys_equal t t')
        | _ -> assert false))
      ~value:(check invariant_a)
      ~upcoming_steps:(check (fun upcoming_steps ->
        assert (List.is_sorted upcoming_steps ~compare:(fun (time1, _) (time2, _) ->
          Time_ns.compare time1 time2));
        List.iter upcoming_steps ~f:(fun (_, a) -> invariant_a a)))
      ~alarm:(check Alarm.invariant))
;;

let advance t ~time_passed =
  let value = ref t.value in
  let upcoming_steps = ref t.upcoming_steps in
  let continue = ref true in
  while !continue do
    match !upcoming_steps with
    | [] -> continue := false
    | (step_at, a) :: rest ->
      if time_passed step_at
      then (value := a; upcoming_steps := rest)
      else continue := false
  done;
  t.value <- !value;
  t.upcoming_steps <- !upcoming_steps;
;;
