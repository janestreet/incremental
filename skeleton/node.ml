open! Core
open Incremental.For_analyzer

type t =
  { id : Node_id.t
  ; kind : Kind.t
  ; children : Node_id.t list [@sexp.list]
  ; bind_children : Node_id.t list [@sexp.list]
  ; user_info : Dot_user_info.t option [@sexp.option]
  ; recomputed_at : Stabilization_num.t
  ; cutoff : Cutoff.t [@default Cutoff.Phys_equal] [@sexp_drop_default.equal]
  ; changed_at : Stabilization_num.t
  ; height : int
  }
[@@deriving sexp]
