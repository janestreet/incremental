open! Core_kernel
open! Import

let () = Incremental_kernel.Private.sexp_of_time_ns := Time_ns.Alternate_sexp.sexp_of_t
let () = Incremental_kernel.Private.sexp_of_time_ns_span := Time_ns.Span.sexp_of_t

module Extra_state = struct
  module Packed_weak_hashtbl = struct
    type t = T : (_, _) Weak_hashtbl.t -> t
    [@@deriving sexp_of]
  end

  type t = Packed_weak_hashtbl.t Thread_safe_queue.t

  let create : unit -> t = Thread_safe_queue.create

  let reclaim_space_in_weak_hashtbls t =
    while Thread_safe_queue.length t > 0 do
      let Packed_weak_hashtbl.T weak_hashtbl =
        Thread_safe_queue.dequeue_exn t
      in
      Weak_hashtbl.reclaim_space_for_keys_with_unused_data weak_hashtbl;
    done;
  ;;
end

include Incremental_intf
module Config = Incremental_kernel.Config
module type Incremental_config = Config.Incremental_config

module Make_with_config (C : Incremental_config) () = struct

  include Incremental_kernel.Make_with_config (struct
      let bind_lhs_change_should_invalidate_rhs = C.bind_lhs_change_should_invalidate_rhs

      (* Make sure [start] is rounded to the nearest microsecond.  Otherwise, if you
         feed [Clock.now ()] to a time function, it can be rounded down to a time in
         the past, causing errors. *)
      let start = Time_ns.of_time (Time_ns.to_time C.start)

      let timing_wheel_config = C.timing_wheel_config
    end) ()

  module Extra_state = struct
    include Extra_state
    let t = create ()
  end

  let stabilize () =
    Extra_state.reclaim_space_in_weak_hashtbls Extra_state.t;
    stabilize ();
  ;;

  let weak_memoize_fun_by_key
        ?(initial_size = default_hash_table_initial_size)
        hashable project_key f =
    let scope = Scope.current () in
    let table = Weak_hashtbl.Using_hashable.create ~size:initial_size hashable in
    let packed = Extra_state.Packed_weak_hashtbl.T table in
    Weak_hashtbl.set_run_when_unused_data table ~thread_safe_f:(fun () ->
      Thread_safe_queue.enqueue Extra_state.t packed);
    stage (fun a ->
      let key = project_key a in
      match Weak_hashtbl.find table key with
      | Some b -> b
      | None ->
        let b = Scope.within scope ~f:(fun () -> f a) in
        Weak_hashtbl.add_exn table ~key ~data:b;
        b)
  ;;

  let weak_memoize_fun ?initial_size hashable f =
    weak_memoize_fun_by_key ?initial_size hashable Fn.id f
  ;;
end

module Make () = Make_with_config (Incremental_kernel.Config.Default ()) ()

module Private = struct
  include Import
end
