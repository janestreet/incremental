open Base

module Runtime = struct
  type 'a test_pred =
    ?here:Lexing.position list -> ?message:string -> ('a -> bool) -> 'a -> unit

  type 'a test_eq =
    ?here:Lexing.position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> 'a
    -> 'a
    -> unit

  type ('a : value_or_null) test_result =
    ?here:Lexing.position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> expect:'a
    -> 'a
    -> unit

  exception E of string * Sexp.t [@@deriving sexp]

  let exn_sexp_style ~message ~pos ~here ~tag body =
    let message =
      match message with
      | None -> tag
      | Some s -> s ^ ": " ^ tag
    in
    let sexp =
      Sexp.List
        (body
         @ [ Sexp.List [ Sexp.Atom "Loc"; Sexp.Atom pos ] ]
         @
         match here with
         | [] -> []
         | _ ->
           [ Sexp.List [ Sexp.Atom "Stack"; [%sexp_of: Source_code_position.t list] here ]
           ])
    in
    (* Here and in other places we return exceptions, rather than directly raising, and
     instead raise at the latest moment possible, so backtrace don't include noise from
     these functions that construct exceptions. *)
    E (message, sexp)
  ;;

  let[@cold] exn_test_pred ~message ~pos ~here ~sexpifier t =
    exn_sexp_style
      ~message
      ~pos
      ~here
      ~tag:"predicate failed"
      [ Sexp.List [ Sexp.Atom "Value"; sexpifier t ] ]
  ;;

  let test_pred ~pos ~sexpifier ~here ?message predicate t =
    if not (predicate t) then raise (exn_test_pred ~message ~pos ~here ~sexpifier t)
  ;;

  let r_diff : (from_:string -> to_:string -> unit) option = None

  let[@cold] test_result_or_eq_failed ~sexpifier ~expect ~got =
    let got = sexpifier got in
    let expect = sexpifier expect in
    (match r_diff with
     | None -> ()
     | Some diff ->
       let from_ = Sexp.to_string_hum expect in
       let to_ = Sexp.to_string_hum got in
       diff ~from_ ~to_);
    `Fail (expect, got)
  ;;

  let test_result_or_eq ~sexpifier ~comparator ~equal ~expect ~got =
    let pass =
      match equal with
      | None -> comparator got expect = 0
      | Some f -> f got expect
    in
    if pass then `Pass else test_result_or_eq_failed ~sexpifier ~expect ~got
  ;;

  let[@cold] exn_test_eq ~message ~pos ~here ~t1 ~t2 =
    exn_sexp_style ~message ~pos ~here ~tag:"comparison failed" [ t1; Sexp.Atom "vs"; t2 ]
  ;;

  let test_eq ~pos ~sexpifier ~comparator ~here ?message ?equal t1 t2 =
    match test_result_or_eq ~sexpifier ~comparator ~equal ~expect:t1 ~got:t2 with
    | `Pass -> ()
    | `Fail (t1, t2) -> raise (exn_test_eq ~message ~pos ~here ~t1 ~t2)
  ;;

  let[@cold] exn_test_result ~message ~pos ~here ~expect ~got =
    exn_sexp_style
      ~message
      ~pos
      ~here
      ~tag:"got unexpected result"
      [ Sexp.List [ Sexp.Atom "expected"; expect ]; Sexp.List [ Sexp.Atom "got"; got ] ]
  ;;

  let[@warning "-16"] test_result
    ~pos
    ~sexpifier
    ~comparator
    ~here
    ?message
    ?equal
    ~expect
    ~got
    =
    match test_result_or_eq ~sexpifier ~comparator ~equal ~expect ~got with
    | `Pass -> ()
    | `Fail (expect, got) -> raise (exn_test_result ~message ~pos ~here ~expect ~got)
  ;;
end
