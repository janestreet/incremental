open Core_kernel
open! Import
open Types.Kind
module Bind = Types.Bind
module Node = Types.Node
module Scope = Types.Scope

type ('a, 'b) t = ('a, 'b) Bind.t =
  { main : 'b Node.t
  ; (* [f] is the user-supplied function that we run each time [t.lhs] changes.  It is
       mutable only so we can clear it when [t] is invalidated. *)
    mutable f : 'a -> 'b Node.t
  ; lhs : 'a Node.t
  ; lhs_change : unit Node.t
  ; (* [rhs] is initially [none], and after that is [some] of the result of the most recent
       call to [f]. *)
    mutable rhs : 'b Node.t Uopt.t
  ; (* [rhs_scope] is the scope in which [t.f] is run, i.e. it is [Scope.Bind t].  It is
       [mutable] only to avoid a [let rec] during creation. *)
    mutable rhs_scope : Scope.t
  ; (* [all_nodes_created_on_rhs] is the head of the singly-linked list of nodes created on
       the right-hand side of [t], i.e. in [t.rhs_scope]. *)
    mutable all_nodes_created_on_rhs : Node.Packed.t Uopt.t
  }
[@@deriving fields, sexp_of]

let same (t1 : (_, _) t) (t2 : (_, _) t) = phys_same t1 t2

let is_valid t =
  match t.main.kind with
  | Invalid -> false
  | _ -> true
;;

let iter_nodes_created_on_rhs t ~(f : Node.Packed.t -> unit) =
  let r = ref t.all_nodes_created_on_rhs in
  while Uopt.is_some !r do
    let (T node_on_rhs) = Uopt.unsafe_value !r in
    r := node_on_rhs.next_node_in_same_scope;
    f (T node_on_rhs)
  done
;;

let invariant _invariant_a _invariant_b t =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:
        (check (fun (main : _ Node.t) ->
           match main.kind with
           | Invalid -> ()
           | Bind_main t' -> assert (same t t')
           | _ -> assert false))
      ~f:ignore
      ~lhs:ignore
      ~lhs_change:
        (check (fun (lhs_change : _ Node.t) ->
           assert (phys_equal lhs_change.created_in t.main.created_in);
           match lhs_change.kind with
           | Invalid -> ()
           | Bind_lhs_change t' -> assert (same t t')
           | _ -> assert false))
      ~rhs:ignore
      ~rhs_scope:
        (check (function
           | Scope.Top -> assert false
           | Bind t' -> assert (same t t')))
      ~all_nodes_created_on_rhs:
        (check (fun _ ->
           iter_nodes_created_on_rhs t ~f:(fun (T node) ->
             assert (phys_equal node.created_in t.rhs_scope);
             if Node.is_necessary node
             then assert (t.lhs_change.height < node.height)))))
;;
