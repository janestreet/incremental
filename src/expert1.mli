(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    This module is almost the external interface of the [Expert], but
    defunctorized, so it's easier to use from the inside of incremental. *)

module Dependency : sig
  type 'a t [@@deriving sexp_of]

  val create : ?on_change:('a -> unit) -> 'a Node.t -> 'a t
  val value : 'a t -> 'a
end

module Node : sig
  type 'a t [@@deriving sexp_of]

  val create
    :  State.t
    -> ?on_observability_change:(is_now_observable:bool -> unit)
    -> (unit -> 'a)
    -> 'a t

  val watch : 'a t -> 'a Node.t
  val make_stale : _ t -> unit
  val invalidate : _ t -> unit
  val add_dependency : _ t -> _ Dependency.t -> unit
  val remove_dependency : _ t -> _ Dependency.t -> unit
end
