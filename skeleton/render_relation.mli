open! Core

type t =
  | All
  | Ancestors
  | Descendants
  | Both
[@@deriving sexp, enumerate, compare, equal]

val arg : t Command.Arg_type.t
