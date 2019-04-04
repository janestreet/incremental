(** A module internal to Incremental. Users should see {!Incremental_intf}.

    A [Reduce_balanced.t] is a kind of DAG node. ['a] is the type of value being folded. *)

open! Core_kernel
open! Import

val create
  :  State.t
  -> 'a Node.t array
  -> f:('a -> 'b)
  -> reduce:('b -> 'b -> 'b)
  -> 'b Node.t option
