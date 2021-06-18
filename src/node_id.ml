open Core
open! Import
include Int

let invariant t = assert (t >= 1)

let next =
  let r = ref 0 in
  fun () ->
    incr r;
    !r
;;
