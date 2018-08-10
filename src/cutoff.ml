open Core_kernel
open! Import

type 'a t =
  (* We specialize some cutoffs to to avoid an indirect function call; in particular we
     specialize the default (and hence overwhelmingly common) case of physical
     equality. *)
  | Always
  | Never
  | Phys_equal
  | Compare of ('a -> 'a -> int)
  | F of (old_value:'a -> new_value:'a -> bool)
[@@deriving sexp_of]

let invariant _ t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    match t with
    | Always     -> ()
    | Never      -> ()
    | Phys_equal -> ()
    | Compare _  -> ()
    | F _        -> ())
;;

let create f = F f

let of_compare f = Compare f

let never  = Never
let always = Always

let poly_equal = F (fun ~old_value ~new_value -> Poly.equal old_value new_value)

let should_cutoff t ~old_value ~new_value =
  match t with
  | Phys_equal -> phys_equal old_value new_value
  | Never -> false
  | Always -> true
  | Compare f -> f old_value new_value = 0
  | F f -> f ~old_value ~new_value
;;

let equal t1 t2 =
  match t1, t2 with
  | Always    , Always     -> true
  | Always    , _          -> false
  | Never     , Never      -> true
  | Never     , _          -> false
  | Phys_equal, Phys_equal -> true
  | Phys_equal, _          -> false
  | Compare f1, Compare f2 -> phys_equal f1 f2
  | Compare _ , _ -> false
  | F f1, F f2 -> phys_equal f1 f2
  | F _, _ -> false
;;

let phys_equal = Phys_equal
