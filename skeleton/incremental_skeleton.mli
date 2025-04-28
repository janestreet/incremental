open! Core
module Node = Node
module Render_relation = Render_relation

type t =
  { nodes : Node.t list
  ; seen : Incremental.For_analyzer.Node_id.Set.t
  ; num_stabilizes : int
  }
[@@deriving sexp]

module Render_target : sig
  type t =
    | Dot
    | Graph_easy
end

(** Creates a static snapshot of the current incremental graph.

    If [?normalize] is true (default false), node IDs will be normalized relative to the
    minimum node ID in the graph. This is primarily useful for tests. *)
val snapshot : ?normalize:bool -> _ Incremental.State.t -> t

(** Converts a [t] to a dot string that can be rendered by graphviz. *)
val to_dot
  :  ?extra_attrs:(Node.t -> Incremental.For_analyzer.Dot_user_info.t option)
  -> ?render_target:Render_target.t
  -> ?filtered_nodes:Node.t list
  -> ?render_relation:Render_relation.t
  -> t
  -> string
