open Core_kernel
open! Import
include Int

let invariant t = assert (t >= -1)
let none = -1
let is_none t = t = none
let is_some t = t >= 0
let add1 t = t + 1
