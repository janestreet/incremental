(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    An on-update handler is stored in a node or an observer, and is run at the end of a
    stabilization either when the value of the node changes, when the handler is
    installed, or when the node becomes invalid. *)

open! Core_kernel
open! Import

module Node_update : sig
  type 'a t =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary
  [@@deriving compare, sexp_of]
end

type 'a t [@@deriving sexp_of]

val create : ('a Node_update.t -> unit) -> at:Stabilization_num.t -> 'a t
val run : 'a t -> 'a Node_update.t -> now:Stabilization_num.t -> unit
