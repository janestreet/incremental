module type S = sig
  type t [@@deriving sexp_of]
end

module type S1 = sig
  type 'a t [@@deriving sexp_of]
end

module type S2 = sig
  type ('a, 'b) t [@@deriving sexp_of]
end
