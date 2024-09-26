open! Core

let%expect_test "render dot file" =
  let module Incr = Incremental.Make () in
  let open Incr.Let_syntax in
  let n1 = Incr.const 1 in
  let n2 = Incr.const 2 in
  let res =
    let%bind n1 in
    if n1 % 2 = 0
    then (
      let%map n2 in
      n1 * n2)
    else n2
  in
  let observer = Incr.observe res in
  Incr.stabilize ();
  let result = Incr.Observer.value_exn observer in
  print_s [%message (result : int)];
  [%expect {| (result 2) |}];
  let skeleton = Incremental_skeleton.snapshot ~normalize:true Incr.State.t in
  print_s [%message (skeleton : Incremental_skeleton.t)];
  [%expect
    {|
    (skeleton
     ((nodes
       (((id 4) (kind Bind_main) (children (3 2)) (recomputed_at 0)
         (changed_at 0) (height 2))
        ((id 3) (kind Bind_lhs_change) (children (1)) (recomputed_at 0)
         (cutoff Never) (changed_at 0) (height 1))
        ((id 1) (kind Const) (recomputed_at 0) (changed_at 0) (height 0))
        ((id 2) (kind Const) (recomputed_at 0) (changed_at 0) (height 0))))
      (seen (1 2 3 4)) (num_stabilizes 1)))
    |}];
  let dot = Incremental_skeleton.to_dot ~render_target:Dot skeleton in
  let grapheasy_dot = Incremental_skeleton.to_dot ~render_target:Graph_easy skeleton in
  Expect_test_patdiff.print_patdiff dot grapheasy_dot;
  [%expect
    {|
    -1,11 +1,11
      digraph G {
        rankdir = TB
        bgcolor = transparent
    -|    n4 [shape=Mrecord label="{{n4|Bind_main|height=2}}"  "fontname"="Sans Serif"]
    +|    n4 [shape=box label="{{n4|Bind_main|height=2}}" ]
    -|    n3 [shape=Mrecord label="{{n3|Bind_lhs_change|height=1}}"  "fontname"="Sans Serif"]
    +|    n3 [shape=box label="{{n3|Bind_lhs_change|height=1}}" ]
    -|    n1 [shape=Mrecord label="{{n1|Const|height=0}}"  "fontname"="Sans Serif"]
    +|    n1 [shape=box label="{{n1|Const|height=0}}" ]
    -|    n2 [shape=Mrecord label="{{n2|Const|height=0}}"  "fontname"="Sans Serif"]
    +|    n2 [shape=box label="{{n2|Const|height=0}}" ]
        n3 -> n4
        n2 -> n4
        n1 -> n3
      }
    |}]
;;

let%expect_test "no binds" =
  let module Incr = Incremental.Make () in
  let node = Incr.return "hello" in
  let node = Incr.map node ~f:(fun x -> x ^ "!") in
  let result = Incr.observe node in
  Incr.stabilize ();
  let result = Incr.Observer.value_exn result in
  print_s [%message (result : string)];
  [%expect {| (result hello!) |}];
  let skeleton = Incremental_skeleton.snapshot ~normalize:true Incr.State.t in
  print_s [%message (skeleton : Incremental_skeleton.t)];
  [%expect
    {|
    (skeleton
     ((nodes
       (((id 2) (kind Map) (children (1)) (recomputed_at 0) (changed_at 0)
         (height 1))
        ((id 1) (kind Const) (recomputed_at 0) (changed_at 0) (height 0))))
      (seen (1 2)) (num_stabilizes 1)))
    |}];
  let dot = Incremental_skeleton.to_dot ~render_target:Graph_easy skeleton in
  print_endline dot;
  [%expect
    {|
    digraph G {
      rankdir = TB
      bgcolor = transparent
        n2 [shape=box label="{{n2|Map|height=1}}" ]
        n1 [shape=box label="{{n1|Const|height=0}}" ]
      n1 -> n2
    }
    |}];
  Expect_test_graphviz.print_dot_blocking dot;
  [%expect
    {|
    ┌───────────────────────┐
    │ {{n1|Const|height=0}} │
    └───────────────────────┘
      │
      │
      ▼
    ┌───────────────────────┐
    │  {{n2|Map|height=1}}  │
    └───────────────────────┘
    |}]
;;

let%expect_test "with binds" =
  let module Incr = Incremental.Make () in
  let open Incr.Let_syntax in
  let mk_incr i =
    let v = Incr.Var.create i in
    Incr.Var.watch v, v
  in
  let a, ai = mk_incr 0 in
  let b, _bi = mk_incr 3 in
  let c, _ci = mk_incr 4 in
  let node =
    let is_even =
      let%map a in
      a % 2 = 0
    in
    let%bind is_even in
    if is_even
    then return 0
    else (
      let%map b and c in
      b * c)
  in
  let observer = Incr.observe node in
  Incr.stabilize ();
  let result = Incr.Observer.value_exn observer in
  print_s [%message (result : int)];
  [%expect {| (result 0) |}];
  let skeleton = Incremental_skeleton.snapshot ~normalize:true Incr.State.t in
  print_s [%message (skeleton : Incremental_skeleton.t)];
  [%expect
    {|
    (skeleton
     ((nodes
       (((id 6) (kind Bind_main) (children (5 7)) (recomputed_at 0)
         (changed_at 0) (height 4))
        ((id 5) (kind Bind_lhs_change) (children (4)) (bind_children (7))
         (recomputed_at 0) (cutoff Never) (changed_at 0) (height 2))
        ((id 4) (kind Map) (children (1)) (recomputed_at 0) (changed_at 0)
         (height 1))
        ((id 1) (kind Var) (recomputed_at 0) (changed_at 0) (height 0))
        ((id 7) (kind Const) (recomputed_at 0) (changed_at 0) (height 3))))
      (seen (1 4 5 6 7)) (num_stabilizes 1)))
    |}];
  Incr.Var.set ai 3;
  Incr.stabilize ();
  let result = Incr.Observer.value_exn observer in
  print_s [%message (result : int)];
  [%expect {| (result 12) |}];
  let new_skeleton = Incremental_skeleton.snapshot ~normalize:true Incr.State.t in
  Expect_test_sexp_diff.print_sexp_diff
    ([%sexp_of: Incremental_skeleton.t] skeleton)
    ([%sexp_of: Incremental_skeleton.t] new_skeleton);
  [%expect
    {|
     ((nodes                      ((nodes
       (((id 6)                     (((id 6)
         (kind Bind_main)             (kind Bind_main)
         (children                    (children
          (5                           (5
    -      7                     +      9
          ))                           ))
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height                      (height
    -     4                      +     5
         ))                           ))
        ((id 5)                      ((id 5)
         (kind Bind_lhs_change)       (kind Bind_lhs_change)
         (children (4))               (children (4))
         (bind_children               (bind_children
    -     (7)                    +     (9 8)
         )                            )
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (cutoff Never)               (cutoff Never)
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 2))                  (height 2))
        ((id 4)                      ((id 4)
         (kind Map)                   (kind Map)
         (children (1))               (children (1))
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 1))                  (height 1))
        ((id 1)                      ((id 1)
         (kind Var)                   (kind Var)
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 0))                  (height 0))
                                 +   ((id 9) (kind Map) (children (8)) (recomputed_at 1) (changed_at 1)
                                 +    (height 4))
        ((id                         ((id
    -     7                      +     8
         )                            )
         (kind                        (kind
    -     Const                  +     Map2
         )                            )
                                 +    (children (2 3))
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 3))                  (height 3))
                                 +   ((id 2) (kind Var) (recomputed_at 1) (changed_at 1) (height 0))
                                 +   ((id 3) (kind Var) (recomputed_at 1) (changed_at 1) (height 0))
       ))                           ))
      (seen                        (seen
       (1                           (1
                                 +   2
                                 +   3
        4                            4
        5                            5
        6                            6
                                 +   8
    -   7                        +   9
       ))                           ))
      (num_stabilizes              (num_stabilizes
    -  1                         +  2
      ))                           ))
    |}];
  let dot = Incremental_skeleton.to_dot ~render_target:Graph_easy skeleton in
  print_endline dot;
  [%expect
    {|
    digraph G {
      rankdir = TB
      bgcolor = transparent
        n6 [shape=box label="{{n6|Bind_main|height=4}}" ]
        n5 [shape=box label="{{n5|Bind_lhs_change|height=2}}" ]
        n4 [shape=box label="{{n4|Map|height=1}}" ]
        n1 [shape=box label="{{n1|Var|height=0}}" ]
        n7 [shape=box label="{{n7|Const|height=3}}" ]
      n5 -> n6
      n7 -> n6
      n4 -> n5
      n1 -> n4
      n5 -> n7 [style=dashed]
    }
    |}];
  Expect_test_graphviz.print_dot_blocking dot;
  [%expect
    {|
    ┌─────────────────────────────────┐
    │       {{n1|Var|height=0}}       │
    └─────────────────────────────────┘
      │
      │
      ▼
    ┌─────────────────────────────────┐
    │       {{n4|Map|height=1}}       │
    └─────────────────────────────────┘
      │
      │
      ▼
    ┌─────────────────────────────────┐
    │ {{n5|Bind_lhs_change|height=2}} │ ─┐
    └─────────────────────────────────┘  │
      ╵                                  │
      ╵                                  │
      ▼                                  │
    ┌─────────────────────────────────┐  │
    │      {{n7|Const|height=3}}      │  │
    └─────────────────────────────────┘  │
      │                                  │
      │                                  │
      ▼                                  │
    ┌─────────────────────────────────┐  │
    │    {{n6|Bind_main|height=4}}    │ ◀┘
    └─────────────────────────────────┘
    |}]
;;

let%expect_test "unobserved incr graph" =
  let module Incr = Incremental.Make () in
  let node = Incr.return "hello" in
  let node = Incr.map node ~f:(fun x -> x ^ "!") in
  let result = Incr.observe node in
  Incr.stabilize ();
  let skeleton = Incremental_skeleton.snapshot ~normalize:true Incr.State.t in
  print_s [%message (skeleton : Incremental_skeleton.t)];
  [%expect
    {|
    (skeleton
     ((nodes
       (((id 2) (kind Map) (children (1)) (recomputed_at 0) (changed_at 0)
         (height 1))
        ((id 1) (kind Const) (recomputed_at 0) (changed_at 0) (height 0))))
      (seen (1 2)) (num_stabilizes 1)))
    |}];
  Incr.Observer.disallow_future_use result;
  Incr.stabilize ();
  let skeleton = Incremental_skeleton.snapshot ~normalize:true Incr.State.t in
  print_s [%message (skeleton : Incremental_skeleton.t)];
  [%expect {| (skeleton ((nodes ()) (seen ()) (num_stabilizes 2))) |}]
;;
