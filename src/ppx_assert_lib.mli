@@ portable

(*_ This is a copy of Ppx_assert_lib.Runtime while I'm waiting for the actual runtime to
    be portable. The reason that the original Ppx_assert_lib isn't portable is because of
    the mutable string diffing function that it uses. *)

open Base

module Runtime : sig
  type 'a test_pred =
    ?here:Lexing.position list -> ?message:string -> ('a -> bool) -> 'a -> unit

  type 'a test_eq =
    ?here:Lexing.position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> 'a
    -> 'a
    -> unit

  type ('a : value_or_null) test_result =
    ?here:Lexing.position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> expect:'a
    -> 'a
    -> unit

  (** Functions called by the generated code *)

  val test_pred
    : ('a : value_or_null).
    pos:string
    -> sexpifier:('a -> Sexp.t)
    -> here:Lexing.position list
    -> ?message:string
    -> ('a -> bool)
    -> 'a
    -> unit

  val test_eq
    : ('a : value_or_null).
    pos:string
    -> sexpifier:('a -> Sexp.t)
    -> comparator:('a -> 'a -> int)
    -> here:Lexing.position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> 'a
    -> 'a
    -> unit

  val test_result
    : ('a : value_or_null).
    pos:string
    -> sexpifier:('a -> Sexp.t)
    -> comparator:('a -> 'a -> int)
    -> here:Lexing.position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> expect:'a
    -> got:'a
    -> unit
end
