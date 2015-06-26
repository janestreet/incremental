open Core.Std
open Import    let _ = _squelch_unused_module_warning_

module Backtrace_string = struct

  type t = string

  let sexp_of_t t = String.split t ~on:'\n' |> <:sexp_of< string list >>

end

type t =
  { exn       : exn
  ; backtrace : Backtrace_string.t
  }
with sexp_of

let create exn =
  { exn
  ; backtrace = Backtrace.Exn.most_recent ()
  }
;;

