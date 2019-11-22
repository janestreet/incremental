(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    The adjust-heights heap is used after an edge is added to the graph from a child node
    to a parent node.  If the child's height is greater than or equal to the parent's
    height, then [Adjust_heights_heap.adjust_heights] increases the height of the parent
    and its ancestors as necessary in order to restore the height invariant.  This is done
    by visiting ancestors in topological order, using the adjust-heights heap to visit
    them in increasing order of pre-adjusted height. *)

open! Core_kernel
open! Import
open Types

type t = Types.Adjust_heights_heap.t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : max_height_allowed:int -> t
val length : t -> int

(** It is required that all nodes have [n.height <= max_height_allowed t].  Any attempt
    to set a node's height larger will raise.

    One can call [set_max_height_allowed] to change the maximum-allowed height.
    [set_max_height_allowed t m] raises if [m < max_height_seen t]. *)
val max_height_allowed : t -> int

val set_max_height_allowed : t -> int -> unit

(** [max_height_seen t] returns the maximum height of any node ever created, not just
    nodes currently in use. *)
val max_height_seen : t -> int

(** [set_height] must be called to change the height of a node, except when clearing the
    height to [-1].  This allows the adjust-heights heap to track the maximum height of
    all nodes.  [set_height] raises if [node.height > max_height_allowed t]. *)
val set_height : t -> _ Node.t -> int -> unit

(** [adjust_heights t recompute_heap ~child ~parent] is called when [parent] is added as a
    parent of [child] and [child.height >= parent.height].  It restores the node height
    invariant: [child.height < parent.height] (for [parent] and all its ancestors).

    Pre and post-conditions:

    - [t] is empty.  Thus, for all nodes [n], [n.height_in_adjust_heights_heap = -1].
    - For all nodes [n] in [recompute_heap], [n.height = n.height_in_recompute_heap].

    [adjust_heights] adds a node [n] to the adjust-heights heap when it detects that
    [c.height >= n.height] for some child [c] of [n].  It adds [n] with
    [n.height_in_adjust_heights_heap] set to the pre-adjusted height of [n], and then sets
    [n.height] to [c.height + 1].  [adjust_heights] then does not change
    [n.height_in_adjust_heights_heap] until [n] is removed from [t], at which point it is
    reset to [-1].  [adjust_heights] may increase [n.height] further as it detects other
    children [c] of [n] with [c.height >= n.height].  A node's [height_in_recompute_heap]
    changes at most once during [adjust_heights], once the node's final adjusted height is
    known.

    Here is the algorithm.

    while [t] is not empty:
    1. remove an [n] in [t] with minimum [n.height_in_adjust_heights_heap].
    2. [Recompute_heap.increase_height recompute_heap n].
    3. for all parents [p] of [n], if [n.height >= p.height], then ensure [p] is in [t]
    and set [p.height] to [n.height + 1] and

    If [adjust_heights] ever encounters [child] while visiting the ancestors of [parent],
    then there is a cycle in the graph and [adjust_heights] raises.

    [adjust_heights] raises if a node's height needs to be increased beyond
    [max_height_allowed t]. *)
val adjust_heights : t -> Recompute_heap.t -> child:_ Node.t -> parent:_ Node.t -> unit
