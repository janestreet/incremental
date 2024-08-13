open Core
open! Import

module String_list = struct
  type t = string list [@@deriving compare, sexp]
  type comparator_witness = String.comparator_witness List.comparator_witness

  let comparator = List.comparator String.comparator
end

type dot =
  { label : Set.M(String_list).t
  ; attributes : string String.Map.t
  }
[@@deriving sexp]

type t =
  | Dot of dot
  | Info of Info.t
  | Append of
      { prior : t
      ; new_ : t
      }
[@@deriving sexp]

let info info = Info info
let append prior new_ = Append { prior; new_ }

let dot ~label ~attributes =
  let label = Set.singleton (module String_list) label in
  Dot { label; attributes }
;;

let rec to_dot = function
  | Info i ->
    { label = Set.singleton (module String_list) [ Info.to_string_hum i ]
    ; attributes = String.Map.empty
    }
  | Dot dot -> dot
  | Append { prior; new_ } ->
    let prior = to_dot prior in
    let new_ = to_dot new_ in
    let label = Set.union prior.label new_.label in
    let attributes =
      Map.merge_skewed
        prior.attributes
        new_.attributes
        ~combine:(fun ~key:_ _left right -> right)
    in
    { label; attributes }
;;

let escape_dot_string s =
  (* https://graphviz.org/doc/info/lang.html *)
  "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\\\"" ^ "\""
;;

let escape_record_label s =
  (* https://graphviz.org/doc/info/shapes.html *)
  String.concat_map s ~f:(function
    | ('<' | '>' | '{' | '}' | '|' | '\\' | ' ') as c -> "\\" ^ String.of_char c
    | c -> String.of_char c)
;;

let to_string ?(shape = "Mrecord") ~name { label; attributes } =
  let label =
    label
    |> Set.to_list
    |> List.map ~f:(fun cols ->
      "{" ^ String.concat (List.map cols ~f:escape_record_label) ~sep:"|" ^ "}")
    |> String.concat ~sep:"|"
    |> fun s -> "{" ^ s ^ "}"
  in
  let attributes =
    attributes
    |> Map.to_alist
    |> List.map ~f:(fun (k, v) ->
      sprintf {| %s=%s|} (escape_dot_string k) (escape_dot_string v))
    |> String.concat ~sep:" "
  in
  sprintf {|  %s [shape=%s label=%s %s]|} name shape (escape_dot_string label) attributes
;;
