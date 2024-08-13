open! Core
module Node_id = Incremental.For_analyzer.Node_id
module Dot_user_info = Incremental.For_analyzer.Dot_user_info

(* Structure for extracting desired information out of an [Incr] graph, to be used for
   further analysis/filtering/visualization *)

type t =
  { nodes : Node.t list
  ; seen : Node_id.Set.t
  ; num_stabilizes : int
  }
[@@deriving sexp]

module Render_target : sig
  type t =
    | Dot
    | Graph_easy
end

val snapshot : ?normalize:bool -> _ Incremental.State.t -> t

val to_dot
  :  ?extra_attrs:(Node.t -> Dot_user_info.t option)
  -> ?render_target:Render_target.t
  -> ?filtered_nodes:Node.t list
  -> ?render_relation:Render_relation.t
  -> t
  -> string
