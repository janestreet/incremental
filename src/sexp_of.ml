module type S = sig
  type t with sexp_of
end

module type S1 = sig
  type 'a t with sexp_of
end

module type S2 = sig
  type ('a, 'b) t with sexp_of
end


