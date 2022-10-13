open Core
open! Import

type t =
  { exn : exn
  ; backtrace : Backtrace.t
  }
[@@deriving sexp_of]

let create exn = { exn; backtrace = Backtrace.Exn.most_recent () }

let reraise_with_message { exn; backtrace } msg =
  Exn.raise_with_original_backtrace (Exn.Reraised (msg, exn)) backtrace
;;

let reraise { exn; backtrace } = Exn.raise_with_original_backtrace exn backtrace
