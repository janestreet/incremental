open Core_kernel
open! Import
module Node = Types.Node

type 'a t = 'a Types.Var.t =
  { mutable value : 'a
  ; (* [value_set_during_stabilization] is only set to [Uopt.some] if the user calls
       [Var.set] during stabilization, in which case it holds the (last) value set.  At
       the end of stabilization, all such variables are processed to do [t.value <-
       t.value_set_during_stabilization]. *)
    mutable value_set_during_stabilization : 'a Uopt.t
  ; (* [set_at] the stabilization number in effect the most recent time [t.value] changed.
       This is not necessarily the same as the stabilization number in effect the most
       recent time [Var.set t] was called, due to the effect of [Var.set] during
       stabilization being delayed until after the stabilization. *)
    mutable set_at : Stabilization_num.t
  ; watch : 'a Node.t
  }
[@@deriving fields, sexp_of]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~value:(check invariant_a)
      ~value_set_during_stabilization:(check (Uopt.invariant invariant_a))
      ~set_at:(check Stabilization_num.invariant)
      ~watch:
        (check (fun (watch : _ Node.t) ->
           match watch.kind with
           | Invalid -> () (* possible with [~use_current_scope:true] *)
           | Var t' -> assert (phys_equal t t')
           | _ -> assert false)))
;;

let incr_state t = t.watch.state

module Packed = struct
  type 'a var = 'a t [@@deriving sexp_of]
  type t = Types.Var.Packed.t = T : _ var -> t [@@unboxed] [@@deriving sexp_of]
end

let latest_value t =
  if Uopt.is_some t.value_set_during_stabilization
  then Uopt.unsafe_value t.value_set_during_stabilization
  else t.value
;;
