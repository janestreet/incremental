open! Core
open Incremental.For_analyzer

(* Snapshot of a [Incr.Node]'s data/metadata for use in analysis/visualization of the
   overall graph *)

type t =
  { id : Node_id.t
  ; kind : Kind.t
  ; children : Node_id.t list
  ; bind_children : Node_id.t list
  ; user_info : Dot_user_info.t option
  ; recomputed_at : Stabilization_num.t
  ; cutoff : Cutoff.t
  ; changed_at : Stabilization_num.t
  ; height : int
  }
[@@deriving sexp]
