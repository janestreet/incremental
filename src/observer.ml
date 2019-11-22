open Core_kernel
open! Import

type 'a t = 'a Internal_observer.t ref [@@deriving sexp_of]

let invariant invariant_a t = Internal_observer.invariant invariant_a !t
let observing t = Internal_observer.observing !t
let use_is_allowed t = Internal_observer.use_is_allowed !t
let value_exn t = Internal_observer.value_exn !t
let incr_state t = Internal_observer.incr_state !t

let on_update_exn t on_update_handler =
  Internal_observer.on_update_exn !t on_update_handler
;;
