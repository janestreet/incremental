open Core.Std
open! Import

type t

let sexp_of_t = [%sexp_of: _]
