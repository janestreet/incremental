(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    In several places, we use [Should_not_use.t] to implement a "lightweight" existential.
    I.e. for some type ['a t], we "pack" it by using [Obj.magic] to cast to
    [Should_not_use.t t].  This is OK so long as we we never mix the [Should_not_use.t]
    parts from two different types that have been so packed.

    We do this for performance reasons, to avoid the boxing and indirection that comes
    from OCaml's implementation of packing.  It can also make pointer-manipulation code
    more transparent.
*)

type t [@@deriving sexp_of]
