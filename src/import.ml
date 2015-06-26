open Core.Std

include Int.Replace_polymorphic_compare

(* All [assert]s and other checks throughout the code are guarded by [if debug].  The
   DEBUG variable is set in the lib [incremental_lib] and unset in the lib
   [incremental_debug], but apart from that they are identical.  Tests are run with both
   the production and debug lib, and users can choose to build with the debug library, if
   they suspect they found a bug in incremental. *)
let debug = IFDEF DEBUG THEN true ELSE false ENDIF

(* All debug messages throughout the code are guarded by [if verbose]. *)
let verbose = false

let concat = String.concat

let tag name a sexp_of_a = (name, a) |> <:sexp_of< string * a >>

let () = Debug.should_print_backtrace := false

module Array = struct
  include Array

  (* Not defining aliases in production mode, since they break type specialization of
     array accesses. *)
  IFDEF DEBUG THEN
    let unsafe_get = get
    let unsafe_set = set
  END
end

let _squelch_unused_module_warning_ = ()
