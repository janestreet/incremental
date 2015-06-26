open Core.Std
open Import

type 'a t = 'a

(* This [Obj.magic] is OK because we never allow user code access to [none] (except via
   [unsafe_value].  [Uopt] is used only internally in [Incremental_lib].  And we never
   have an [_ Uopt.t Uopt.t], so there is no chance of confusing [none] with [some
   none].  And we never build [float Uopt.t array]s. *)
let none = (Obj.magic ("Uopt.none" : string) : _ t)

let is_none t = phys_equal t none

let is_some t = not (is_none t)

let invariant invariant_a t = if is_some t then invariant_a t

let sexp_of_t sexp_of_a t =
  if is_none t
  then "None" |> <:sexp_of< string >>
  else tag "Some" t <:sexp_of< a >>
;;

let some a = a

let value_exn t = if is_none t then failwith "Uopt.value_exn" else t

let unsafe_value t = if debug then value_exn t else t

TEST = is_none none
TEST = not (is_some none)
TEST = not (is_none (some 13))
TEST = is_some (some 13)
TEST = Exn.does_raise (fun () -> value_exn none)
TEST = value_exn (some 13) = 13
TEST = unsafe_value (some 13) = 13
