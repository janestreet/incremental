open! Core_kernel

module Time_ns = struct
  include Time_ns

  let sexp_of_t = Time_ns.Alternate_sexp.sexp_of_t
end

type 'a t =
  { init : 'a
  ; steps : (Time_ns.t * 'a) Sequence.t
  }
[@@deriving fields, sexp_of]

let invariant invariant_a { init; steps = _ } = invariant_a init

let rec value_internal init steps ~at =
  match Sequence.next steps with
  | None -> init
  | Some ((t, a), steps) ->
    if Time_ns.( < ) at t then init else value_internal a steps ~at
;;

let value t ~at = value_internal t.init t.steps ~at
let constant init = { init; steps = Sequence.empty }

let create_exn ~init ~steps =
  if not
       (List.is_sorted steps ~compare:(fun (time1, _) (time2, _) ->
          Time_ns.compare time1 time2))
  then
    raise_s
      [%message
        "[Step_function.create_exn] got unsorted times"
          ~steps:(steps |> List.map ~f:fst : Time_ns.t list)];
  { init; steps = steps |> Sequence.of_list }
;;

let create_from_sequence ~init ~steps = { init; steps }
