open Core_kernel
open! Import
open Types.Internal_observer

module Packed_ = struct
  include Types.Internal_observer.Packed

  let sexp_of_t (T internal_observer) =
    internal_observer.observing |> [%sexp_of: _ Types.Node.t]
  ;;

  let prev_in_all (T t) = t.prev_in_all
  let next_in_all (T t) = t.next_in_all
  let set_prev_in_all (T t1) t2 = t1.prev_in_all <- t2
  let set_next_in_all (T t1) t2 = t1.next_in_all <- t2
end

module State = struct
  type t = Types.Internal_observer.State.t =
    | Created
    | In_use
    | Disallowed
    | Unlinked
  [@@deriving sexp_of]
end

type 'a t = 'a Types.Internal_observer.t =
  { (* State transitions:

       {v
         Created --> In_use --> Disallowed --> Unlinked
           |                                     ^
           \-------------------------------------/
       v} *)
    mutable state : State.t
  ; observing : 'a Node.t
  ; mutable on_update_handlers : 'a On_update_handler.t list
  ; (* [{prev,next}_in_all] doubly link all observers in [state.all_observers]. *)
    mutable prev_in_all : Packed_.t Uopt.t
  ; mutable next_in_all : Packed_.t Uopt.t
  ; (* [{prev,next}_in_observing] doubly link all observers of [observing]. *)
    mutable prev_in_observing : ('a t[@sexp.opaque]) Uopt.t
  ; mutable next_in_observing : ('a t[@sexp.opaque]) Uopt.t
  }
[@@deriving fields, sexp_of]

type 'a internal_observer = 'a t [@@deriving sexp_of]

let incr_state t = t.observing.state

let use_is_allowed t =
  match t.state with
  | Created | In_use -> true
  | Disallowed | Unlinked -> false
;;

let same (t1 : _ t) (t2 : _ t) = phys_same t1 t2
let same_as_packed (t1 : _ t) (Packed_.T t2) = same t1 t2

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~state:ignore
      ~observing:(check (Node.invariant invariant_a))
      ~on_update_handlers:
        (check (fun on_update_handlers ->
           match t.state with
           | Created | In_use | Disallowed -> ()
           | Unlinked -> assert (List.is_empty on_update_handlers)))
      ~prev_in_all:
        (check (fun prev_in_all ->
           (match t.state with
            | In_use | Disallowed -> ()
            | Created | Unlinked -> assert (Uopt.is_none prev_in_all));
           if Uopt.is_some prev_in_all
           then
             assert (
               same_as_packed
                 t
                 (Uopt.value_exn (Packed_.next_in_all (Uopt.value_exn prev_in_all))))))
      ~next_in_all:
        (check (fun next_in_all ->
           (match t.state with
            | In_use | Disallowed -> ()
            | Created | Unlinked -> assert (Uopt.is_none next_in_all));
           if Uopt.is_some next_in_all
           then
             assert (
               same_as_packed
                 t
                 (Uopt.value_exn (Packed_.prev_in_all (Uopt.value_exn next_in_all))))))
      ~prev_in_observing:
        (check (fun prev_in_observing ->
           (match t.state with
            | In_use | Disallowed -> ()
            | Created | Unlinked -> assert (Uopt.is_none prev_in_observing));
           if Uopt.is_some prev_in_observing
           then
             assert (
               phys_equal
                 t
                 (Uopt.value_exn
                    (next_in_observing (Uopt.value_exn prev_in_observing))))))
      ~next_in_observing:
        (check (fun next_in_observing ->
           (match t.state with
            | In_use | Disallowed -> ()
            | Created | Unlinked -> assert (Uopt.is_none next_in_observing));
           if Uopt.is_some next_in_observing
           then
             assert (
               phys_equal
                 t
                 (Uopt.value_exn
                    (prev_in_observing (Uopt.value_exn next_in_observing)))))))
;;

let value_exn t =
  match t.state with
  | Created ->
    failwiths
      ~here:[%here]
      "Observer.value_exn called without stabilizing"
      t
      [%sexp_of: _ t]
  | Disallowed | Unlinked ->
    failwiths
      ~here:[%here]
      "Observer.value_exn called after disallow_future_use"
      t
      [%sexp_of: _ t]
  | In_use ->
    let uopt = t.observing.value_opt in
    if Uopt.is_none uopt
    then
      failwiths ~here:[%here] "attempt to get value of an invalid node" t [%sexp_of: _ t];
    Uopt.unsafe_value uopt
;;

let on_update_exn t on_update_handler =
  match t.state with
  | Disallowed | Unlinked ->
    failwiths ~here:[%here] "on_update disallowed" t [%sexp_of: _ t]
  | Created | In_use ->
    t.on_update_handlers <- on_update_handler :: t.on_update_handlers;
    (match t.state with
     | Disallowed | Unlinked -> assert false
     | Created ->
       (* We'll bump [observing.num_on_update_handlers] when [t] is actually added to
          [observing.observers] at the start of the next stabilization. *)
       ()
     | In_use ->
       let observing = t.observing in
       observing.num_on_update_handlers <- observing.num_on_update_handlers + 1)
;;

let unlink_from_observing t =
  let prev = t.prev_in_observing in
  let next = t.next_in_observing in
  t.prev_in_observing <- Uopt.none;
  t.next_in_observing <- Uopt.none;
  if Uopt.is_some next then (Uopt.unsafe_value next).prev_in_observing <- prev;
  if Uopt.is_some prev then (Uopt.unsafe_value prev).next_in_observing <- next;
  let observing = t.observing in
  if phys_equal t (Uopt.value_exn observing.observers) then observing.observers <- next;
  observing.num_on_update_handlers
  <- observing.num_on_update_handlers - List.length t.on_update_handlers;
  t.on_update_handlers <- []
;;

let unlink_from_all t =
  let prev = t.prev_in_all in
  let next = t.next_in_all in
  t.prev_in_all <- Uopt.none;
  t.next_in_all <- Uopt.none;
  if Uopt.is_some next then Packed_.set_prev_in_all (Uopt.unsafe_value next) prev;
  if Uopt.is_some prev then Packed_.set_next_in_all (Uopt.unsafe_value prev) next
;;

let unlink t =
  unlink_from_observing t;
  unlink_from_all t
;;

module Packed = struct
  include Packed_

  let sexp_of_t (T internal_observer) =
    internal_observer |> [%sexp_of: _ internal_observer]
  ;;

  let invariant (T t) = invariant ignore t
end
