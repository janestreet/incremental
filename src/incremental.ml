(* This module is mostly a wrapper around [State] functions. *)

open! Core_kernel
open! Import
include Incremental_intf

module type Incremental_config = Config.Incremental_config

module Config = Config

let default_max_height_allowed = 128

module Generic = struct
  module Cutoff = Cutoff
  module Step_function = Step_function

  module State = struct
    include State

    module type S = sig
      type state_witness [@@deriving sexp_of]

      val t : t
    end

    let create_internal = create

    let create ?(max_height_allowed = default_max_height_allowed) () : (module S) =
      (module struct
        type state_witness [@@deriving sexp_of]

        let t = create (module Config.Default ()) ~max_height_allowed
      end)
    ;;
  end

  module Scope = struct
    include Scope

    let current (state : State.t) () = state.current_scope
    let within state t ~f = State.within_scope state t ~f
  end

  include Node
  module Node_update = On_update_handler.Node_update

  let state t = t.state
  let pack t = Packed.T t
  let const state a = State.const state a
  let return = const
  let observe = State.create_observer
  let map = State.map
  let bind = State.bind

  module N_ary_map_and_bind = struct
    let map2 = State.map2
    let map3 = State.map3
    let map4 = State.map4
    let map5 = State.map5
    let map6 = State.map6
    let map7 = State.map7
    let map8 = State.map8
    let map9 = State.map9
    let map10 = State.map10
    let map11 = State.map11
    let map12 = State.map12
    let map13 = State.map13
    let map14 = State.map14
    let map15 = State.map15
    let bind2 = State.bind2
    let bind3 = State.bind3
    let bind4 = State.bind4
  end

  include N_ary_map_and_bind

  module Infix = struct
    let ( >>| ) t f = map t ~f
    let ( >>= ) t f = bind t ~f
  end

  include Infix

  let join = State.join
  let if_ = State.if_
  let lazy_from_fun state f = State.lazy_from_fun state ~f
  let default_hash_table_initial_size = State.default_hash_table_initial_size
  let memoize_fun_by_key = State.memoize_fun_by_key

  let memoize_fun ?initial_size state hashable f =
    memoize_fun_by_key state ?initial_size hashable Fn.id f
  ;;

  let array_fold state ts ~init ~f = State.array_fold state ts ~init ~f
  let reduce_balanced state ts ~f ~reduce = Reduce_balanced.create state ts ~f ~reduce

  module Unordered_array_fold_update = State.Unordered_array_fold_update

  let unordered_array_fold = State.unordered_array_fold
  let opt_unordered_array_fold = State.opt_unordered_array_fold
  let all = State.all
  let exists = State.exists
  let for_all = State.for_all
  let both t1 t2 = map2 t1 t2 ~f:Tuple2.create
  let sum = State.sum
  let opt_sum = State.opt_sum
  let sum_int = State.sum_int
  let sum_float = State.sum_float

  module Var = struct
    include Var

    let create = State.create_var
    let set = State.set_var
    let value t = t.value
    let watch t = t.watch

    (* We override [sexp_of_t] to just show the value, rather than the internal
       representation. *)
    let sexp_of_t sexp_of_a t = t.value |> [%sexp_of: a]
    let replace t ~f = set t (f (latest_value t))
  end

  module Observer = struct
    include Observer

    module Update = struct
      type 'a t =
        | Initialized of 'a
        | Changed of 'a * 'a
        | Invalidated
      [@@deriving compare, sexp_of]
    end

    let on_update_exn t ~(f : _ Update.t -> unit) =
      State.observer_on_update_exn t ~f:(function
        | Necessary a -> f (Initialized a)
        | Changed (a1, a2) -> f (Changed (a1, a2))
        | Invalidated -> f Invalidated
        | Unnecessary ->
          failwiths
            ~here:[%here]
            "Incremental bug -- Observer.on_update_exn got unexpected update \
             Unnecessary"
            t
            [%sexp_of: _ t])
    ;;

    let disallow_future_use t = State.disallow_future_use !t
    let value = State.observer_value
    let value_exn = State.observer_value_exn

    (* We override [sexp_of_t] to just show the value, rather than the internal
       representation. *)
    let sexp_of_t sexp_of_a (t : _ t) =
      match !t.state with
      | Created -> [%message "<unstabilized>"]
      | Disallowed | Unlinked -> [%message "<disallowed>"]
      | In_use ->
        let uopt = !t.observing.value_opt in
        if Uopt.is_none uopt
        then [%message "<invalid>"]
        else [%sexp (Uopt.unsafe_value uopt : a)]
    ;;
  end

  module Before_or_after = Before_or_after

  module Clock = struct
    include State.Clock

    let default_timing_wheel_config =
      let alarm_precision = Alarm_precision.about_one_millisecond in
      let level_bits = [ 14; 13; 5 ] in
      Timing_wheel.Config.create
        ~alarm_precision
        ~level_bits:
          (Timing_wheel.Level_bits.create_exn level_bits ~extend_to_max_num_bits:true)
        ()
    ;;

    let create state ?(timing_wheel_config = default_timing_wheel_config) ~start () =
      (* Make sure [start] is rounded to the nearest microsecond.  Otherwise, if you
         feed [Clock.now ()] to a time function, it can be rounded down to a time in
         the past, causing errors. *)
      let start =
        Time_ns.of_time_float_round_nearest_microsecond
          (Time_ns.to_time_float_round_nearest_microsecond start)
      in
      State.create_clock state ~timing_wheel_config ~start
    ;;

    let alarm_precision t = Timing_wheel.alarm_precision t.timing_wheel
    let timing_wheel_length = State.timing_wheel_length
    let now = State.now
    let watch_now t = t.now.watch
    let at = State.at
    let after = State.after
    let at_intervals = State.at_intervals
    let advance_clock = State.advance_clock
    let advance_clock_by t span = advance_clock t ~to_:(Time_ns.add (now t) span)
    let incremental_step_function = State.incremental_step_function

    let step_function t ~init steps =
      incremental_step_function
        t
        (const (incr_state t) (Step_function.create_exn ~init ~steps))
    ;;

    let snapshot = State.snapshot
  end

  let freeze ?(when_ = fun _ -> true) t = State.freeze t ~only_freeze_when:when_
  let depend_on t ~depend_on = State.depend_on t ~depend_on
  let necessary_if_alive = State.necessary_if_alive

  module Update = On_update_handler.Node_update

  let on_update = State.node_on_update
  let stabilize state = State.stabilize state
  let am_stabilizing state = State.am_stabilizing state
  let save_dot = State.save_dot

  module Node_value = struct
    type 'a t =
      | Invalid
      | Necessary_maybe_stale of 'a option
      | Unnecessary_maybe_stale of 'a option
    [@@deriving sexp_of]
  end

  let node_value t : _ Node_value.t =
    if not (is_valid t)
    then Invalid
    else if is_necessary t
    then Necessary_maybe_stale (Uopt.to_option t.value_opt)
    else Unnecessary_maybe_stale (Uopt.to_option t.value_opt)
  ;;

  (* We override [sexp_of_t] to show just the value, rather than the internal
     representation.  We only show the value if it is necessary and valid. *)
  let sexp_of_t sexp_of_a t =
    if not (is_valid t)
    then "<invalid>" |> [%sexp_of: string]
    else if not (is_necessary t)
    then "<unnecessary>" |> [%sexp_of: string]
    else if Uopt.is_none t.value_opt
    then "<uncomputed>" |> [%sexp_of: string]
    else unsafe_value t |> [%sexp_of: a]
  ;;

  module Expert = Expert1

  module Let_syntax = struct
    let return = return
    let ( >>| ) = ( >>| )
    let ( >>= ) = ( >>= )

    module Let_syntax = struct
      let bind = bind
      let map = map
      let both t1 t2 = map2 t1 t2 ~f:(fun x1 x2 -> x1, x2)

      include N_ary_map_and_bind

      module Open_on_rhs = struct
        let watch = Var.watch
      end
    end
  end

  let weak_memoize_fun_by_key = State.weak_memoize_fun_by_key

  let weak_memoize_fun ?initial_size state hashable f =
    weak_memoize_fun_by_key ?initial_size state hashable Fn.id f
  ;;
end

module Make_with_config (Incremental_config : Incremental_config) () = struct
  type state_witness [@@deriving sexp_of]

  include Generic

  module State = struct
    include State

    let t = create_internal (module Incremental_config) ~max_height_allowed:128
  end

  module Clock = struct
    include Clock

    let create ?timing_wheel_config ~start () =
      create ?timing_wheel_config State.t ~start ()
    ;;
  end

  module Expert = struct
    include Expert

    module Node = struct
      include Node

      let create ?on_observability_change f = create State.t ?on_observability_change f
    end
  end

  module Let_syntax = struct
    include Let_syntax

    let return a = return State.t a
  end

  module Scope = struct
    include Scope

    let current () = current State.t ()
    let within t ~f = within State.t t ~f
  end

  module Var = struct
    include Var

    let create ?use_current_scope value = create ?use_current_scope State.t value
  end

  let const a = const State.t a
  let return a = return State.t a
  let all ts = all State.t ts
  let exists ts = exists State.t ts
  let for_all ts = for_all State.t ts
  let lazy_from_fun state f = State.lazy_from_fun state ~f

  let memoize_fun_by_key ?initial_size hashable project_key f =
    memoize_fun_by_key ?initial_size State.t hashable project_key f
  ;;

  let memoize_fun ?initial_size hashable f = memoize_fun ?initial_size State.t hashable f
  let array_fold ts ~init ~f = array_fold State.t ts ~init ~f
  let reduce_balanced ts ~f ~reduce = reduce_balanced State.t ts ~f ~reduce

  let unordered_array_fold ?full_compute_every_n_changes ts ~init ~f ~update =
    unordered_array_fold State.t ts ~init ~f ~update ?full_compute_every_n_changes
  ;;

  let opt_unordered_array_fold ?full_compute_every_n_changes ts ~init ~f ~f_inverse =
    opt_unordered_array_fold ?full_compute_every_n_changes State.t ts ~init ~f ~f_inverse
  ;;

  let sum ?full_compute_every_n_changes ts ~zero ~add ~sub =
    sum ?full_compute_every_n_changes State.t ts ~zero ~add ~sub
  ;;

  let opt_sum ?full_compute_every_n_changes ts ~zero ~add ~sub =
    opt_sum ?full_compute_every_n_changes State.t ts ~zero ~add ~sub
  ;;

  let sum_int ts = sum_int State.t ts
  let sum_float ts = sum_float State.t ts
  let stabilize () = stabilize State.t
  let am_stabilizing () = am_stabilizing State.t
  let save_dot file = save_dot State.t file
  let lazy_from_fun f = lazy_from_fun State.t f

  let weak_memoize_fun_by_key ?initial_size hashable project_key f =
    weak_memoize_fun_by_key ?initial_size State.t hashable project_key f
  ;;

  let weak_memoize_fun ?initial_size hashable f =
    weak_memoize_fun ?initial_size State.t hashable f
  ;;
end

module Make () = Make_with_config (Config.Default ()) ()
include Generic

module Add_witness0 (M : sig
    type t [@@deriving sexp_of]

    include Invariant.S with type t := t
  end) : sig
  type 'w t = M.t [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t
end = struct
  type 'w t = M.t

  let invariant _ t = M.invariant t
  let sexp_of_t _ t = M.sexp_of_t t
end

module Add_witness1 (M : sig
    type 'a t [@@deriving sexp_of]

    include Invariant.S1 with type 'a t := 'a t
  end) : sig
  type ('a, 'w) t = 'a M.t [@@deriving sexp_of]

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t
end = struct
  type ('a, 'w) t = 'a M.t

  let invariant invariant_a _ t = M.invariant invariant_a t
  let sexp_of_t sexp_of_a _ t = M.sexp_of_t sexp_of_a t
end

module Clock = struct
  include Clock
  include Add_witness0 (Clock)
end

module Expert = struct
  include Expert

  module Dependency = struct
    include Dependency

    include Add_witness1 (struct
        include Dependency

        let invariant _ _ = ()
      end)
  end

  module Node = struct
    include Node

    include Add_witness1 (struct
        include Node

        let invariant _ _ = ()
      end)
  end
end

module Node = struct
  include Node
  include Add_witness1 (Node)
end

type ('a, 'w) t = ('a, 'w) Node.t [@@deriving sexp_of]
type ('a, 'w) incremental = ('a, 'w) t

let invariant = Node.invariant

module Observer = struct
  include Observer
  include Add_witness1 (Observer)
end

module Scope = struct
  include Scope
  include Add_witness0 (Scope)
end

module State = struct
  include State
  include Add_witness0 (State)
end

module Var = struct
  include Var
  include Add_witness1 (Var)
end

module type S = sig
  type state_witness [@@deriving sexp_of]

  include
    S_gen
    with type 'a t = ('a, state_witness) incremental
    with type Before_or_after.t = Before_or_after.t
    with type Clock.t = state_witness Clock.t
    with type 'a Cutoff.t = 'a Cutoff.t
    with type 'a Expert.Dependency.t = ('a, state_witness) Expert.Dependency.t
    with type 'a Expert.Node.t = ('a, state_witness) Expert.Node.t
    with type 'a Observer.t = ('a, state_witness) Observer.t
    with type 'a Observer.Update.t = 'a Observer.Update.t
    with type Packed.t = Packed.t
    with type Scope.t = state_witness Scope.t
    with type State.t = state_witness State.t
    with type State.Stats.t = State.Stats.t
    with type ('a, 'b) Unordered_array_fold_update.t =
           ('a, 'b) Unordered_array_fold_update.t
    with type 'a Update.t = 'a Update.t
    with type 'a Var.t = ('a, state_witness) Var.t
end

module Private = struct
  let debug = debug
end
