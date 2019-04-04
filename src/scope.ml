open Core_kernel
open! Import
module Node = Types.Node
include Types.Scope

let top = Top

let is_top = function
  | Top -> true
  | Bind _ -> false
;;

let invariant = function
  | Top -> ()
  | Bind bind -> Bind.invariant ignore ignore bind
;;

(* Unlike for nodes, there is no invariant [is_necessary t <=> height > -1] (doesn't work
   because of [Top]).  This is fine since the height of a scope is only used to constrain
   other heights, not to schedule it. *)
let height = function
  | Top -> -1
  | Bind bind -> bind.lhs_change.height
;;

let is_valid = function
  | Top -> true
  | Bind bind -> Bind.is_valid bind
;;

let is_necessary = function
  | Top -> true
  | Bind bind -> Node.is_necessary bind.main
;;

let add_node t (node : _ Node.t) =
  assert (phys_equal node.created_in t);
  match t with
  | Top -> ()
  | Bind bind ->
    node.next_node_in_same_scope <- bind.all_nodes_created_on_rhs;
    bind.all_nodes_created_on_rhs <- Uopt.some (Types.Node.Packed.T node)
;;
