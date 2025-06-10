open Core
open! Import

type 'a t =
  (* We specialize some cutoffs to avoid an indirect function call; in particular we
     specialize the default (and hence overwhelmingly common) case of physical
     equality. *)
  | Always
  | Never
  | Phys_equal
  | Compare of ('a -> 'a -> int)
  | Equal of ('a -> 'a -> bool)
  | F of (old_value:'a -> new_value:'a -> bool)
[@@deriving sexp_of]

let invariant _ t =
  Invariant.invariant t [%sexp_of: _ t] (fun () ->
    match t with
    | Always | Never | Phys_equal | Compare _ | Equal _ | F _ -> ())
;;

let create f = F f
let of_compare f = Compare f
let of_equal f = Equal f
let get_never () = Never
let never = Never
let always = Always
let get_always () = Always
let poly_equal = Equal Poly.equal

let should_cutoff t ~old_value ~new_value =
  match t with
  | Phys_equal -> phys_equal old_value new_value
  | Never -> false
  | Always -> true
  | Compare f -> f old_value new_value = 0
  | Equal f -> f old_value new_value
  | F f -> f ~old_value ~new_value
;;

let equal t1 t2 =
  match t1, t2 with
  | Always, Always -> true
  | Always, _ -> false
  | Never, Never -> true
  | Never, _ -> false
  | Phys_equal, Phys_equal -> true
  | Phys_equal, _ -> false
  | Compare f1, Compare f2 -> phys_equal f1 f2
  | Compare _, _ -> false
  | Equal f1, Equal f2 -> phys_equal f1 f2
  | Equal _, _ -> false
  | F f1, F f2 -> phys_equal f1 f2
  | F _, _ -> false
;;

let phys_equal = Phys_equal

module For_analyzer = struct
  type 'a t' = 'a t

  type t =
    | Always
    | Never
    | Phys_equal
    | Compare
    | Equal
    | F
  [@@deriving sexp, equal]

  let of_cutoff (c : _ t') =
    match c with
    | Always -> Always
    | Never -> Never
    | Phys_equal -> Phys_equal
    | Compare _ -> Compare
    | Equal _ -> Equal
    | F _ -> F
  ;;

  let to_string t = Sexp.to_string ([%sexp_of: t] t)
end
