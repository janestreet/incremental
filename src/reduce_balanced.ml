open! Core_kernel
open! Import

let create state children ~f ~reduce =
  let len = Array.length children in
  if len = 0
  then None
  else (
    let reducer = Balanced_reducer.create_exn () ~len ~reduce in
    if debug then Balanced_reducer.invariant (const ()) reducer;
    let node =
      Expert1.Node.create state (fun () ->
        let a = Balanced_reducer.compute_exn reducer in
        if debug then Balanced_reducer.invariant (const ()) reducer;
        a)
    in
    for i = 0 to len - 1 do
      Expert1.Node.add_dependency
        node
        (Expert1.Dependency.create children.(i) ~on_change:(fun a ->
           Balanced_reducer.set_exn reducer i (f a);
           if debug then Balanced_reducer.invariant (const ()) reducer))
    done;
    Some (Expert1.Node.watch node))
;;
