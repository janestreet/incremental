[%%import "debug.mlh"]

open Core_kernel
include Int.Replace_polymorphic_compare

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

module Alarm_precision = Timing_wheel.Alarm_precision
