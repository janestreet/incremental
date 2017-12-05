open Core

let () = Incremental_kernel.Private.Import.sexp_of_time_ns := Time_ns.sexp_of_t
let () = Incremental_kernel.Private.Import.sexp_of_time_ns_span := Time_ns.Span.sexp_of_t

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

  include Incremental_kernel.Incremental.Make_with_config (struct
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

  let now () = Time_ns.to_time (now ())

  let watch_now =
    let shared = map (watch_now ()) ~f:Time_ns.to_time in
    fun () -> shared
  ;;

  let advance_clock ~to_ = advance_clock ~to_:(Time_ns.of_time to_)

  let at time = at (Time_ns.of_time time)

  let after span = after (Time_ns.Span.of_span span)

  let at_intervals span = at_intervals (Time_ns.Span.of_span span)

  let step_function ~init alist =
    step_function ~init (List.map alist ~f:(fun (time, a) -> (Time_ns.of_time time, a)))
  ;;

  let snapshot t ~at ~before = snapshot t ~at:(Time_ns.of_time at) ~before
end

module Make () = Make_with_config (Incremental_kernel.Config.Default ()) ()
