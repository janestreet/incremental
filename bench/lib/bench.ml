open Core.Std
open Import

module I = Incremental_lib.Std.Incremental.Make ()
open I
let () = State.set_max_height_allowed State.t 10_000
let ignore = Fn.ignore
module Bench = Core_bench.Std.Bench
module Test = Bench.Test

let rec keep_alive_for_gc x =
  if Random.int 2 = -1 then keep_alive_for_gc x
;;

let stabilize_test =
  [ Test.create ~name:"stabilize" (fun () -> stabilize ()) ]
;;

let bind_return =
  let var = Var.create 0 in
  let node = bind (Var.watch var) return in
  let obs = observe node in
  let name = "bind-return" in
  let r = ref 0 in
  [ Test.create ~name (fun () ->
     incr r;
     Var.set var !r;
     stabilize ();
     ignore (Observer.value_exn obs))
  ]
;;

let map_or_bind4 ~which =
  let i0 = Var.create 0 in
  let v0 = Var.watch i0 in
  let i1 = Var.create 0 in
  let v1 = Var.watch i1 in
  let i2 = Var.create 0 in
  let v2 = Var.watch i2 in
  let i3 = Var.create 0 in
  let v3 = Var.watch i3 in
  let node =
    match which with
    | `Map -> map4 v0 v1 v2 v3 ~f:(fun v0 v1 v2 v3 -> v0 + v1 + v2 + v3)
    | `Bind ->
      bind v0 (fun v0 ->
      bind v1 (fun v1 ->
      bind v2 (fun v2 ->
      bind v3 (fun v3 ->
      return (v0 + v1 + v2 + v3)
      ))))
  in
  let obs = observe node in
  let name =
    match which with
    | `Map -> "map4"
    | `Bind -> "bind4"
  in
  let r = ref 0 in
  List.mapi [i0; i1; i2; i3] ~f:(fun i var ->
    let name = name ^ "-" ^ string_of_int i in
    Test.create ~name (fun () ->
      incr r;
      Var.set var !r;
      stabilize ();
      ignore (Observer.value_exn obs)))
;;

let allocate_long_chain size elt =
  let var = Var.create elt in
  let node = Var.watch var in
  let prev = ref node in
  for _i = 0 to size do
    prev := map !prev ~f:Fn.id
  done;
  let obs = observe !prev in
  var, obs
;;

let maybe_batch_computations sizes =
  List.map [ `Batch; `Dont_batch ] ~f:(fun which ->
    let name =
      match which with
      | `Batch -> "batched"
      | `Dont_batch -> "unbatched" in
    let test size =
      let var1, obs1 = allocate_long_chain size 0 in
      let var2, obs2 = allocate_long_chain size 0 in
      let fresh = let r = ref 0 in fun () -> incr r; !r in
      stabilize ();
      match which with
      | `Batch ->
        stage (fun () ->
          Var.set var1 (fresh ());
          Var.set var2 (fresh ());
          stabilize ();
          ignore (Observer.value_exn obs1);
          ignore (Observer.value_exn obs2);
        )
      | `Dont_batch ->
        stage (fun () ->
          Var.set var1 (fresh ());
          stabilize ();
          Var.set var2 (fresh ());
          stabilize ();
          ignore (Observer.value_exn obs1);
          ignore (Observer.value_exn obs2);
        )
    in
    Test.create_indexed ~name ~args:sizes test)
;;

let build_long_chain sizes =
  [ Test.create_indexed ~args:sizes ~name:"build_long_chain" (fun size ->
     stage (fun () ->
       let var, obs = allocate_long_chain size 0 in
       stabilize ();
       keep_alive_for_gc var;
       ignore (Observer.value_exn obs);
       (*Observer.disallow_future_use obs;*)
     ))
  ]
;;

let build_tree depths =
  [ Test.create_indexed ~name:"build_tree" ~args:depths (fun depth ->
     stage (fun () ->
       let rec aux depth =
         if depth = 0
         then Var.watch (Var.create ())
         else map2 (aux (depth - 1)) (aux (depth - 1)) ~f:(fun () () -> ())
       in
       let o = observe (aux depth) in
       stabilize ();
       Observer.value_exn o;
       (*Observer.disallow_future_use o;*)
       Gc.full_major ();
     ))
  ]
;;

let build_dag sizes =
  [ Test.create_indexed ~name:"build_dag" ~args:sizes (fun size ->
     stage (fun () ->
       let rec aux size =
         if size = 0 then Var.watch (Var.create ()) else
           let child = aux (size - 1) in
           map2 child child ~f:(fun () () -> ()) in
       let o = observe (aux size) in
       stabilize ();
       Observer.value_exn o;
       (*Observer.disallow_future_use o;*)
       Gc.full_major (); (* counting gc time in the allocation cost *)
     ))]
;;

let propagate_tree name value f depth =
  let rec aux_no_incremental depth =
    if depth = 0
    then value
    else
      let down1 = aux_no_incremental (depth - 1) in
      let down2 = aux_no_incremental (depth - 1) in
      fun () -> f (value ()) (down1 ()) (down2 ()) in
  let no_incremental = aux_no_incremental depth in
  let vars =
    List.init (depth + 1) ~f:(fun _ ->
      let var = Var.create (value ()) in
      var, Var.watch var)
  in
  let rec aux vars =
    match vars with
    | [] -> assert false
    | [(_, i)] -> i
    | (_, i) :: vars ->
      map3 i (aux vars) (aux vars) ~f
  in
  let obs = observe (aux vars) in
  stabilize ();
  Test.create ~name:(concat ["prop-"; name; "-scratch" ])
    (fun () -> ignore (no_incremental ()))
  ::
  List.mapi vars ~f:(fun depth_of_v (v, _i) ->
    Test.create
      ~name:(concat [ "prop-"; name; "-"; Int.to_string (1 lsl depth_of_v)])
      (fun () ->
        Var.set v (value ());
        stabilize ();
        ignore (Observer.value_exn obs)
      ))
;;

let propagate_dag ~cutoff name value f size =
  let rec aux_no_incremental size =
    if size = 0
    then value
    else
      let down = aux_no_incremental (size - 1) in
      fun () ->
        let v = down () in
        f (value ()) v v
  in
  let no_incremental = aux_no_incremental size in
  let vars =
    List.init (size + 1) ~f:(fun _ ->
      let var = Var.create (value ()) in
      var, Var.watch var)
  in
  let rec aux vars =
    match vars with
    | [] -> assert false
    | [(_, i)] -> i
    | (_, i) :: vars ->
      let res = aux vars in
      let res = map3 i res res ~f in
      I.set_cutoff res
        (match cutoff with
         | `Phys_equal -> I.Cutoff.phys_equal
         | `Equal      -> I.Cutoff.poly_equal
         | `None       -> I.Cutoff.never);
      res
  in
  let obs = observe (aux vars) in
  stabilize ();
  (*let name = "prop-dag-" ^ name in*)
  let name = ignore name; "prop-dag" in
  let name =
    match cutoff with
    | `Equal -> name ^ "-cut"
    | `Phys_equal -> name ^ "-cutq"
    | `None -> name
  in
  Test.create ~name:(name ^ "-scratch")
    (fun () -> ignore (no_incremental ()))
  ::
  List.filter_mapi vars ~f:(fun size_v (v, _i) ->
    if size_v > 0 && not (Int.is_pow2 size_v)
    then None
    else
      Some (
        Test.create
          ~name:(concat [name; "-"; Int.to_string size_v])
          (fun () ->
             Var.set v (value ());
             stabilize ();
             ignore (Observer.value_exn obs);
          )
      ))
;;

let propagate_binds lengths =
  [ Test.create_indexed ~name:"prop-bind" ~args:lengths (fun length ->
     let a =
       Array.init length ~f:(fun i ->
         let var = Var.create i in
         var, Var.watch var)
     in
     let vars  = Array.map a ~f:fst in
     let nodes = Array.map a ~f:snd in
     let rec nest_bind index =
       if index = Array.length nodes then const ()
       else bind nodes.(index) (fun _ -> nest_bind (index + 1))
     in
     let node = nest_bind 0 in
     let obs = observe node in
     let counter = ref 0 in
     stage (fun () ->
       let slices = 100 in
       for i = 0 to slices - 1 do
         let index = i * length / slices in
         Var.set vars.(index) !counter;
         incr counter;
         stabilize ();
       done;
       (* hum, if the gc is runned between every round, then v1 is cheating *)
       Observer.value_exn obs
     ))]
;;

let bench_all ~mine n =
  let l =
    List.init n ~f:(fun i ->
      let var = Var.create i in
      var, Var.watch var)
  in
  let rec my_all = function
    | [] ->
      const []
    | v1 :: [] ->
      map v1 ~f:(fun v1 -> v1 :: [])
    | v1 :: v2 :: [] ->
      map2 v1 v2 ~f:(fun v1 v2 -> v1 :: v2 :: [])
    | v1 :: v2 :: v3 :: [] ->
      map3 v1 v2 v3 ~f:(fun v1 v2 v3 -> v1 :: v2 :: v3 :: [])
    | v1 :: v2 :: v3 :: v4 :: [] ->
      map4 v1 v2 v3 v4 ~f:(fun v1 v2 v3 v4 -> v1 :: v2 :: v3 :: v4 :: [])
    | v1 :: v2 :: v3 :: v4 :: v5 :: l ->
      map6 v1 v2 v3 v4 v5 (my_all l)
        ~f:(fun v1 v2 v3 v4 v5 l -> v1 :: v2 :: v3 :: v4 :: v5 :: l)
  in
  let obs =
    if mine then observe (my_all (List.map l ~f:snd))
    else observe (all (List.map l ~f:snd)) in
  let vars =
    let a = Array.map ~f:fst (Array.of_list l) in [
      a.(0), "head";
      a.(n / 2), "middle";
      a.(n - 1), "tail"
    ] in
  let fresh = let r = ref 0 in fun () -> incr r; !r in
  let name = if mine then "all-mine" else "all" in
  List.map vars ~f:(fun (var, suffix) ->
    let name = name ^ "-" ^ suffix in
    Test.create ~name (fun () ->
      Var.set var (fresh ());
      stabilize ();
      ignore (Observer.value_exn obs)
    )
  )
;;

let leaf =
  let x = Random.int 3 in
  (* preventing caml from allocating the constructor statically because then phys_equal
     works and v4 starts shortcutting some computations *)
  fun () -> `Leaf x
;;

let leaf' =
  let x = ref (-1) in
  fun () -> incr x; !x
;;

let report_error f =
  try
    f ()
  with exn ->
    Debug.eprints "bench construction failed"
      (exn, String.split (Backtrace.Exn.most_recent ()) ~on:'\n')
      <:sexp_of< exn * string list >>;
    exit 1;
;;

let slow_command =
  report_error (fun () ->
    let tests  =
      let _ = bench_all in
      let _ = bind_return in
      let _ = build_dag in
      let _ = build_long_chain in
      let _ = build_tree in
      let _ = leaf in
      let _ = leaf' in
      let _ = map_or_bind4 in
      let _ = maybe_batch_computations in
      let _ = propagate_binds in
      let _ = propagate_dag in
      let _ = propagate_tree in
      let _ = stabilize_test in
      List.concat
        [ stabilize_test
        ; bind_return
        ; map_or_bind4 ~which:`Map
        ; map_or_bind4 ~which:`Bind
        ; maybe_batch_computations [ 100; 1000 ]
        ; build_long_chain [ 100; 1_000; 2_000; 3_000 ]
        ; build_tree (List.init 8 ~f:Fn.id)
        ; build_dag (List.init 8 ~f:(fun i -> 1 lsl (i + 1)))
        ; propagate_tree "cons" leaf' (fun t1 t2 t3 -> t1 + t2 + t3) 6
        ; propagate_dag "cons" ~cutoff:`None leaf
            (fun t1 t2 t3 -> `Cons (t1, t2, t3))
            (1 lsl 8)
        ; propagate_dag "cons" ~cutoff:`Phys_equal leaf
            (fun t1 t2 t3 -> `Cons (t1, t2, t3))
            (1 lsl 8)
        ; propagate_dag "cons" ~cutoff:`Equal leaf
            (fun t1 t2 _t3 ->
               (* comparison doesn't see the sharing so it blows up if we put both t2 and t3 *)
               `Cons (t1, t2, ()))
            (1 lsl 8)
        ; propagate_binds [ 10; 20; 30; 40 ]
        ; bench_all ~mine:true 100
        ; bench_all ~mine:false 100
        ]
    in
    (* Many things are created at toplevel and we don't them to slow down the first
       stabilization. *)
    let () = stabilize () in
    Bench.make_command tests)
;;

let propagate_map1_test size =
  let x = Var.create 0 in
  let rec loop i =
    if i = 0
    then Var.watch x
    else loop (i - 1) >>| fun a -> a + 1
  in
  let observer = observe (loop size) in
  stage (fun () ->
    Var.set x (Var.value x + 1);
    stabilize ();
    assert (Observer.value_exn observer = Var.value x + size))
;;

let propagate_map2_test depth =
  let x = Var.create 0 in
  let rec loop depth =
    if depth = 0
    then Var.watch x
    else map2 (loop (depth - 1)) (loop (depth - 1)) ~f:(+)
  in
  let observer = observe (loop depth) in
  let num_nodes = Float.iround_exn (2. ** Float.of_int depth) in
  stage (fun () ->
    Var.set x (Var.value x + 1);
    stabilize ();
    assert (Observer.value_exn observer = Var.value x * num_nodes))
;;

let propagate_map2_chain_test size =
  let x = Var.create 0 in
  let y = Var.create 0 in
  let rec loop i =
    if i = 0
    then Var.watch x
    else map2 (loop (i - 1)) (Var.watch y) ~f:(fun a b -> a + b)
  in
  let observer = observe (loop size) in
  Var.set y 1;
  stage (fun () ->
    Var.set x (Var.value x + 1);
    stabilize ();
    assert (Observer.value_exn observer = Var.value x + size))
;;

let quick_command =
  report_error (fun () ->
    Bench.make_command
      ( [ Test.create_indexed ~name:"propagate_map1" ~args:[ 1_000 ] propagate_map1_test
        ; Test.create_indexed ~name:"propagate_map2" ~args:[ 10    ] propagate_map2_test
        ; Test.create_indexed ~name:"propagate_map2_chain" ~args:[ 1_000 ] propagate_map2_chain_test
        ]
        @ stabilize_test
      ))
;;

let no_bench =
  Command.basic ~summary:"no bench"
    Command.Spec.empty
    (fun () ->
       let f = unstage (propagate_map1_test 1_000) in
       for _i = 1 to 100_000 do
         f ();
       done
    )
;;

let command =
  Command.group
    ~summary:"benchmarking"
    [ "slow", slow_command
    ; "quick", quick_command
    ; "no-bench", no_bench
    ]
;;
