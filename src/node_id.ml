open Core
open! Import
include Int

let invariant t = assert (t >= 1)

let next =
  let r = Core.Atomic.make 0 in
  fun () -> Core.Atomic.fetch_and_add r 1 + 1
;;
