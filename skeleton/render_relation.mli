open! Core

type t =
  | All
  | Ancestors
  | Descendants
  | Both
[@@deriving sexp, enumerate, compare ~localize, equal ~localize]

val arg : t Command.Arg_type.t
