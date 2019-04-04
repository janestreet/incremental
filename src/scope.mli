(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A scope is a bind in which nodes are created.  It is either [top], for nodes not in a
    bind, or [Uopt.some packed_bind] for nodes created on the right-hand side of a
    bind. *)

open! Core_kernel
open! Import

include module type of struct
  include Types.Scope
end

include Invariant.S with type t := t

val top : t
val is_top : t -> bool
val height : t -> int
val is_valid : t -> bool
val is_necessary : t -> bool
val add_node : t -> _ Types.Node.t -> unit
