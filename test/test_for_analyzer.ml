open! Core
open Import
open Incremental.For_analyzer

type node_print =
  { id : Node_id.t
  ; kind : Kind.t
  ; cutoff : Cutoff.t
  ; children : Node_id.t list
  ; bind_children : Node_id.t list
  ; recomputed_at : Stabilization_num.t
  ; changed_at : Stabilization_num.t
  ; height : int
  }
[@@deriving sexp_of]

let print_nodes node_list pack output_node =
  let nodes = ref [] in
  traverse
    (List.map node_list ~f:pack)
    ~add_node:
      (fun
        ~id
        ~kind
        ~cutoff
        ~children
        ~bind_children
        ~user_info:_
        ~recomputed_at
        ~changed_at
        ~height
      ->
      nodes
      := { id; kind; cutoff; children; bind_children; recomputed_at; changed_at; height }
         :: !nodes);
  let min_id_opt =
    List.map !nodes ~f:(fun node -> Node_id.to_string node.id |> int_of_string)
    |> List.min_elt ~compare:Int.compare
  in
  match min_id_opt with
  | None -> ()
  | Some min_id ->
    let new_id node_id =
      let id_num = (Node_id.to_string node_id |> int_of_string) - min_id + 1 in
      Node_id.t_of_sexp [%sexp (id_num : int)]
    in
    List.iter !nodes ~f:(fun node ->
      let node =
        let new_children = List.map node.children ~f:new_id in
        let new_bind_children = List.map node.bind_children ~f:new_id in
        { node with
          id = new_id node.id
        ; children = new_children
        ; bind_children = new_bind_children
        }
      in
      output_node node)
;;

let print_node node = print_s [%message (node : node_print)]

let print_computation_info node =
  let id = node.id in
  let recomputed_at = node.recomputed_at in
  let changed_at = node.changed_at in
  print_s
    [%message
      (id : Node_id.t)
        (recomputed_at : Stabilization_num.t)
        (changed_at : Stabilization_num.t)]
;;

let%expect_test "traverses basic" =
  let module Incr = Incremental.Make () in
  let n = Incr.return "hello" in
  print_nodes [ n ] Incr.pack print_node;
  [%expect
    {|
    (node (
      (id     1)
      (kind   Const)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    |}]
;;

let%expect_test "traverses map" =
  let module Incr = Incremental.Make () in
  let n = Incr.return "hello" in
  let r = Incr.map n ~f:(fun s -> s ^ "!") in
  print_nodes [ r ] Incr.pack print_node;
  [%expect
    {|
    (node (
      (id     1)
      (kind   Const)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    (node (
      (id     2)
      (kind   Map)
      (cutoff Phys_equal)
      (children (1))
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    |}]
;;

let%expect_test "traverses bind" =
  let module Incr = Incremental.Make () in
  let x = Incr.Var.create 1 in
  let a = Incr.return 3 in
  let b = Incr.return 4 in
  let cond = Incr.map (Incr.Var.watch x) ~f:(fun i -> i % 2 = 0) in
  let c =
    Incr.bind cond ~f:(fun bool -> if bool then a else Incr.map b ~f:(fun i -> i * 4))
  in
  print_nodes [ c ] Incr.pack print_node;
  [%expect
    {|
    (node (
      (id     1)
      (kind   Var)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    (node (
      (id     4)
      (kind   Map)
      (cutoff Phys_equal)
      (children (1))
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    (node (
      (id     5)
      (kind   Bind_lhs_change)
      (cutoff Never)
      (children (4))
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    (node (
      (id     6)
      (kind   Bind_main)
      (cutoff Phys_equal)
      (children (5))
      (bind_children ())
      (recomputed_at -1)
      (changed_at    -1)
      (height        -1)))
    |}];
  let observer_so_that_stabilization_performs_work = Incr.observe c in
  Incr.stabilize ();
  let (_ : _ Incr.Observer.t) =
    Sys.opaque_identity observer_so_that_stabilization_performs_work
  in
  print_nodes [ c ] Incr.pack print_node;
  [%expect
    {|
    (node (
      (id     3)
      (kind   Const)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        0)))
    (node (
      (id     7)
      (kind   Map)
      (cutoff Phys_equal)
      (children (3))
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        3)))
    (node (
      (id     1)
      (kind   Var)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        0)))
    (node (
      (id     4)
      (kind   Map)
      (cutoff Phys_equal)
      (children (1))
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        1)))
    (node (
      (id     5)
      (kind   Bind_lhs_change)
      (cutoff Never)
      (children      (4))
      (bind_children (7))
      (recomputed_at 0)
      (changed_at    0)
      (height        2)))
    (node (
      (id     6)
      (kind   Bind_main)
      (cutoff Phys_equal)
      (children (5 7))
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        4)))
    |}]
;;

let%expect_test "different recomputed_at and changed_at" =
  let module Incr = Incremental.Make () in
  let a = Incr.Var.create 3 in
  let a_val = Incr.Var.watch a in
  let mult = Incr.map a_val ~f:(fun a -> a % 2) in
  let mult_observer = Incr.observe mult in
  Incr.stabilize ();
  print_nodes [ mult ] Incr.pack print_computation_info;
  [%expect
    {|
    ((id            1)
     (recomputed_at 0)
     (changed_at    0))
    ((id            2)
     (recomputed_at 0)
     (changed_at    0))
    |}];
  Incr.Var.set a 1;
  Incr.stabilize ();
  (* NOTE: The reason that [recomputed_at] and [changed_at] are different here, is that -
     according to the doc comments of incremental:

     - [recomputed_at] is the last stabilization when [t]'s value was recomputed, even if
       it was cut off.
     - [changed_at] is the last stabilization when this node was computed and not cut off.
       It is used to detect when [t]'s parents are stale and (because all parents are
       necessary) need to be recomputed.

     The modulo node was "recomputed" (3 % 2) and (1 % 2), but it did not "change" as the resulting
     value was the "same" according to its phys_equal cutoff. *)
  let (_ : _ Incr.Observer.t) = Sys.opaque_identity mult_observer in
  print_nodes [ mult ] Incr.pack print_computation_info;
  [%expect
    {|
    ((id            1)
     (recomputed_at 1)
     (changed_at    1))
    ((id            2)
     (recomputed_at 1)
     (changed_at    0))
    |}]
;;

let%expect_test "directly observes all observers" =
  let module Incr = Incremental.Make () in
  let a = Incr.return "hello" in
  let b = Incr.map a ~f:(fun s -> s ^ "!") in
  let a' = Incr.return "world" in
  let b' = Incr.map a' ~f:(fun s -> s ^ ".") in
  let b_observer = Incr.observe b in
  let b'_observer = Incr.observe b' in
  print_nodes (directly_observed Incr.State.t) Fn.id print_node;
  [%expect {| |}];
  Incr.stabilize ();
  let (_ : _ Incr.Observer.t) = Sys.opaque_identity b_observer in
  let (_ : _ Incr.Observer.t) = Sys.opaque_identity b'_observer in
  print_nodes (directly_observed Incr.State.t) Fn.id print_node;
  [%expect
    {|
    (node (
      (id     1)
      (kind   Const)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        0)))
    (node (
      (id     2)
      (kind   Map)
      (cutoff Phys_equal)
      (children (1))
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        1)))
    (node (
      (id     3)
      (kind   Const)
      (cutoff Phys_equal)
      (children      ())
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        0)))
    (node (
      (id     4)
      (kind   Map)
      (cutoff Phys_equal)
      (children (3))
      (bind_children ())
      (recomputed_at 0)
      (changed_at    0)
      (height        1)))
    |}]
;;
