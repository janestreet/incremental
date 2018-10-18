open Core_kernel
open! Import

type t =
  { exn : exn
  ; backtrace : Backtrace.t
  }
[@@deriving sexp_of]

let create exn = { exn; backtrace = Backtrace.Exn.most_recent () }
