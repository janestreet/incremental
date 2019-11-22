(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A [Node.t] is one node in the incremental DAG.  The key invariants of a node [t] are:

    - if [is_necessary t], then [t.height > c.height], for all children [c] of [t].
    - if [is_necessary t], then [t.height > Scope.height t.created_in].
    - [is_necessary p] for all parents [p] of [t].
    - [t.height < p.height] for all parents [p] of [t].
    - [needs_to_be_computed t = is_in_recompute_heap t].

    Outside of stabilization, when the recompute heap is empty, the invariant implies that
    if [is_necessary t], then [t.recomputed_at >= c.changed_at] for all children [c] of
    [t].  I.e. it implies that all necessary nodes aren't stale. *)

open! Core_kernel
open! Import

(** For performance reasons, we do not use an OCaml existential type for [Node.Packed.t]:

    {[
      type t = T : _ Node.t -> t
    ]}

    The extra indirection when following pointers to packed nodes would be too slow.

    Consequently, there is a possible bug in which we mix the ['a] from two packed nodes
    with different types.  We reduce the chance of this bug by minimizing the scopes in
    which we deal with packed nodes. *)
module Packed : sig
  type t = Types.Node.Packed.t = T : _ Types.Node.t -> t
  [@@unboxed] [@@deriving sexp_of]

  include Invariant.S with type t := t

  (** [As_list] allows one to view a node as a list w.r.t. a particular [next] pointer
      contained within it.  The recompute heap uses this with [next_in_recompute_heap],
      and the adjust-heights heap uses this with [next_in_adjust_heights_heap]. *)
  module As_list (M : sig
      val next : t -> t Uopt.t
    end) : sig
    type t = Types.Node.Packed.t Uopt.t [@@deriving sexp_of]

    include Invariant.S with type t := t

    val length : t -> int
    val iter : t -> f:(Types.Node.Packed.t -> unit) -> unit
  end

  (** [iter_descendants ts ~f] calls [f] on every node in [ts] and all of their
      descendants exactly once per node. *)
  val iter_descendants : t list -> f:(t -> unit) -> unit

  val save_dot : string -> t list -> unit
end

include module type of struct
  include Types.Node
end
with module Packed := Types.Node.Packed

include Invariant.S1 with type 'a t := 'a t

val create : Types.State.t -> Scope.t -> 'a Kind.t -> 'a t

(** One should only set the kind of a node using [set_kind] -- using [t.kind <-] will
    violate invariants. *)
val set_kind : 'a t -> 'a Kind.t -> unit

val same : _ t -> _ t -> bool

(** [iteri_children t ~f] applies [f] to all children of [t]. *)
val iteri_children : _ t -> f:(int -> Packed.t -> unit) -> unit

(*_
  (** [iteri_parents  t ~f] applies [f] to all necessary parents of [t]. *)
  val iteri_parents  : _ t -> f:(int -> Packed.t -> unit) -> unit *)

(** [get_parent t ~index] raises unless [0 <= index < t.num_parents]. *)
val get_parent : _ t -> index:int -> Packed.t

val add_parent : child:'a t -> parent:'b t -> child_index:int -> unit
val remove_parent : child:'a t -> parent:'b t -> child_index:int -> unit

val swap_children_except_in_kind
  :  _ t
  -> child1:_ t
  -> child_index1:int
  -> child2:_ t
  -> child_index2:int
  -> unit

val is_const : _ t -> bool
val is_in_recompute_heap : _ t -> bool

(** [is_necessary t] iff [t] is a descendant of an observer or [t] is a [Freeze] node. *)
val is_necessary : _ t -> bool

(** [is_valid t] returns [true] iff the left-hand-side of [t]'s defining bind hasn't
    changed since [t] was created. *)
val is_valid : _ t -> bool

(** [should_be_invalidated t] returns [true] iff [t] has an invalid child that implies
    that [t] should be invalid.  It doesn't take into account [t.created_in]. *)
val should_be_invalidated : _ t -> bool

(** [edge_is_stale] returns [true] iff [child] has changed since [parent] was computed,
    and implies [is_stale parent].  [edge_is_stale] is constant-time. *)
val edge_is_stale : child:_ t -> parent:_ t -> bool

(** [is_stale t] is true if [t] has never been computed or if some child changed since [t]
    was last computed.  [is_stale] doesn't take into account [t.created_in]. *)
val is_stale : _ t -> bool

(** [needs_to_be_computed] is [is_necessary t && is_stale t] *)
val needs_to_be_computed : _ t -> bool

(** Getting the value of a node.

    [value_exn t] raises iff [Uopt.is_none t.value_opt].
    [unsafe_value t] is safe iff [Uopt.is_some t.value_opt]. *)
val value_exn : 'a t -> 'a

val unsafe_value : 'a t -> 'a
val get_cutoff : 'a t -> 'a Cutoff.t
val set_cutoff : 'a t -> 'a Cutoff.t -> unit

(** [on_update t on_update_handler] adds an on-update handler to [t]. *)
val on_update : 'a t -> 'a On_update_handler.t -> unit

(** [run_on_update_handlers t node_update ~now] runs [t]'s on-update handlers, except
    those created at the stabilization [now]. *)
val run_on_update_handlers
  :  'a t
  -> 'a On_update_handler.Node_update.t
  -> now:Stabilization_num.t
  -> unit

val user_info : _ t -> Info.t option
val set_user_info : _ t -> Info.t option -> unit

(** These functions are meant for debug, as they are not very efficient. *)
val has_child : _ t -> child:_ t -> bool

val has_parent : _ t -> parent:_ t -> bool
