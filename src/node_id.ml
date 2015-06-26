open Core.Std
open Import    let () = _squelch_unused_module_warning_

include Int

let invariant t = assert (t >= 1)

let next =
  let r = ref 0 in
  fun () ->
    incr r;
    !r
;;
