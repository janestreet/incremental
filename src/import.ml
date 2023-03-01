[%%import "debug.mlh"]

open Core

(* All [assert]s and other checks throughout the code are guarded by [if debug].  The
   DEBUG variable is set in the lib [incremental] and unset in the lib
   [incremental_debug], but apart from that they are identical.  Tests are run with both
   the production and debug lib, and users can choose to build with the debug library, if
   they suspect they found a bug in incremental. *)

[%%if JSC_DEBUG]

let debug = true

[%%else]

let debug = false

[%%endif]

let concat = String.concat
let tag name a sexp_of_a = (name, a) |> [%sexp_of: string * a]

module Step_function = Incremental_step_function

module Time_ns = struct
  include Time_ns

  let sexp_of_t = Time_ns.Alternate_sexp.sexp_of_t
end

module Array = struct
  include Array

  (* Not defining aliases in production mode, since they break type specialization of
     array accesses. *)
  [%%if JSC_DEBUG]

  let unsafe_get = get
  let unsafe_set = set

  [%%endif]

  (* Requires [len >= length t]. *)
  let realloc t ~len a =
    let new_t = create ~len a in
    Array.blit ~src:t ~src_pos:0 ~dst:new_t ~dst_pos:0 ~len:(length t);
    new_t
  ;;
end

module Uopt = struct
  include Uopt

  let unsafe_value = if debug then value_exn else unsafe_value
end

module Uniform_array = struct
  include Uniform_array

  [%%if JSC_DEBUG]

  let unsafe_get = get
  let unsafe_set = set_with_caml_modify

  [%%else]

  (* Uniform_array is being "smart" by checking if elements are integers, but Uopt.t
     almost never contain integers, so the extra check to make generated code harder to
     read and potentially slower. *)
  let unsafe_set = unsafe_set_with_caml_modify
  let set = set_with_caml_modify

  [%%endif]

  (* Requires [len >= length t]. *)
  let realloc t ~len =
    let new_t = create ~len Uopt.none in
    blit ~src:t ~src_pos:0 ~dst:new_t ~dst_pos:0 ~len:(length t);
    new_t
  ;;
end

module Alarm_precision = Timing_wheel.Alarm_precision
