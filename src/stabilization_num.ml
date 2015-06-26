open Core.Std
open Import    let _ = _squelch_unused_module_warning_

include Int

let invariant t = assert (t >= -1)

let none = -1

let is_none t = t = none

let is_some t = t >= 0

let add1 t = t + 1
