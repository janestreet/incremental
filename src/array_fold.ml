open Core_kernel
open! Import
module Node = Types.Node

type ('a, 'acc) t = ('a, 'acc) Types.Array_fold.t =
  { init : 'acc
  ; f : 'acc -> 'a -> 'acc
  ; children : 'a Node.t array
  }
[@@deriving fields, sexp_of]

let invariant invariant_a invariant_acc t =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~init:(check invariant_acc)
      ~f:ignore
      ~children:
        (check (fun children ->
           Array.iter children ~f:(fun (child : _ Node.t) ->
             Uopt.invariant invariant_a child.value_opt))))
;;

let compute { init; f; children } =
  let result = ref init in
  for i = 0 to Array.length children - 1 do
    result := f !result (Uopt.value_exn (Array.unsafe_get children i).value_opt)
  done;
  !result
;;
