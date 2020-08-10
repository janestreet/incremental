open! Core_kernel
open! Import

module Previous_update_kind = struct
  type t =
    | Never_been_updated
    | Necessary
    | Changed
    | Invalidated
    | Unnecessary
  [@@deriving sexp_of]
end

module Node_update = struct
  type 'a t =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary
  [@@deriving compare, sexp_of]
end

type 'a t =
  { f : 'a Node_update.t -> unit
  ; mutable previous_update_kind : Previous_update_kind.t
  ; created_at : Stabilization_num.t
  }
[@@deriving sexp_of]

let create f ~at:created_at = { f; previous_update_kind = Never_been_updated; created_at }

let really_run t (node_update : _ Node_update.t) =
  t.previous_update_kind
  <- (match node_update with
    | Necessary _ -> Necessary
    | Changed _ -> Changed
    | Invalidated -> Invalidated
    | Unnecessary -> Unnecessary);
  t.f node_update
;;

let run t (node_update : _ Node_update.t) ~now =
  (* We only run the handler if was created in an earlier stabilization cycle.  If the
     handler was created by another on-update handler during the running of on-update
     handlers in the current stabilization, we treat the added handler as if it were added
     after this stabilization finished.  We will run it at the next stabilization, because
     the node with the handler was pushed on [state.handle_after_stabilization]. *)
  if Stabilization_num.compare t.created_at now < 0
  then (
    match t.previous_update_kind, node_update with
    (* Once a node is invalidated, there will never be further information to provide,
       since incremental does not allow an invalid node to become valid. *)
    | Invalidated, _ -> ()
    (* These cases can happen if a node is handled after stabilization due to another
       handler.  But for the current handler, there is nothing to do because there is no
       new information to provide. *)
    | Changed, Necessary _ | Necessary, Necessary _ | Unnecessary, Unnecessary -> ()
    (* If this handler hasn't seen a node that is changing, we treat the update as an
       initialization. *)
    | (Never_been_updated | Unnecessary), Changed (_, a) -> really_run t (Necessary a)
    (* All other updates are run as is. *)
    | Never_been_updated, (Necessary _ | Unnecessary | Invalidated)
    | Unnecessary, (Necessary _ | Invalidated)
    | Necessary, (Changed _ | Unnecessary | Invalidated)
    | Changed, (Changed _ | Unnecessary | Invalidated) -> really_run t node_update)
;;
