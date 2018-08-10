open Core_kernel
open Import
open Types.Kind

module Node = Types.Node

type ('a, 'acc) t = ('a, 'acc) Types.Unordered_array_fold.t =
  { main                                        : 'acc Node.t
  ; init                                        : 'acc
  ; f                                           : ('acc -> 'a -> 'acc)
  ; f_inverse                                   : ('acc -> 'a -> 'acc)
  ; full_compute_every_n_changes                : int
  ; children                                    : 'a Node.t array
  ; mutable fold_value                          : 'acc Uopt.t
  ; mutable num_changes_since_last_full_compute : int
  }
[@@deriving fields, sexp_of]

let same (t1 : (_, _) t) (t2  : (_, _) t) = phys_same t1 t2

let invariant invariant_a invariant_acc t =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~main:(check (fun (main : _ Node.t) ->
        match main.kind with
        | Invalid -> ()
        | Unordered_array_fold t' -> assert (same t t')
        | _ -> assert false))
      ~init:(check invariant_acc)
      ~f:ignore
      ~f_inverse:ignore
      ~children:(check (fun children ->
        Array.iter children ~f:(fun (child : _ Node.t) ->
          Uopt.invariant invariant_a child.value_opt;
          if t.num_changes_since_last_full_compute < t.full_compute_every_n_changes
          then assert (Uopt.is_some child.value_opt))))
      ~fold_value:(check (fun fold_value ->
        Uopt.invariant invariant_acc fold_value;
        [%test_result: bool] (Uopt.is_some fold_value)
          ~expect:(t.num_changes_since_last_full_compute
                   < t.full_compute_every_n_changes)))
      ~num_changes_since_last_full_compute:
        (check (fun num_changes_since_last_full_compute ->
           assert (num_changes_since_last_full_compute >= 0);
           assert (num_changes_since_last_full_compute <= t.full_compute_every_n_changes)))
      ~full_compute_every_n_changes:(check (fun full_compute_every_n_changes ->
        assert (full_compute_every_n_changes > 0))))
;;

let create ~init ~f ~f_inverse ~full_compute_every_n_changes ~children ~main =
  { init
  ; f
  ; f_inverse
  ; full_compute_every_n_changes
  ; children
  ; main
  ; fold_value                          = Uopt.none
  (* We make [num_changes_since_last_full_compute = full_compute_every_n_changes]
     so that there will be a full computation the next time the node is computed. *)
  ; num_changes_since_last_full_compute = full_compute_every_n_changes
  }
;;

let full_compute { init; f; children; _ } =
  let result = ref init in
  for i = 0 to Array.length children - 1 do
    result := f !result (Uopt.value_exn (Array.unsafe_get children i).value_opt);
  done;
  !result
;;

let compute t =
  if verbose
  then Debug.ams [%here] "Unordered_array_fold.compute" t [%sexp_of: (_, _) t];
  if t.num_changes_since_last_full_compute = t.full_compute_every_n_changes then begin
    t.num_changes_since_last_full_compute <- 0;
    t.fold_value <- Uopt.some (full_compute t);
  end;
  Uopt.value_exn t.fold_value
;;

let force_full_compute t =
  t.fold_value <- Uopt.none;
  t.num_changes_since_last_full_compute <- t.full_compute_every_n_changes;
;;

let child_changed t ~old_value_opt ~new_value =
  if t.num_changes_since_last_full_compute < t.full_compute_every_n_changes - 1
  then begin
    t.num_changes_since_last_full_compute <- t.num_changes_since_last_full_compute + 1;
    t.fold_value <- Uopt.some (t.f (t.f_inverse
                                      (Uopt.value_exn t.fold_value)
                                      (Uopt.value_exn old_value_opt))
                                 new_value);
  end else if t.num_changes_since_last_full_compute < t.full_compute_every_n_changes
  then force_full_compute t;
;;
