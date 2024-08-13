open! Core

type t =
  | All
  | Ancestors
  | Descendants
  | Both
[@@deriving sexp, enumerate, compare, equal]

let arg = Command.Arg_type.create (fun s -> s |> Sexp.Atom |> t_of_sexp)
