@@ portable

(** A module internal to Incremental. Users should see {!Incremental_intf}.

    A type describing user-controlled metadata optionally attached to nodes, usually for
    the purpose of debugging or inspecting the incremental graph. The core of incremental
    never uses this. *)

open Core
open! Import

type dot =
  { label : (string list, String.comparator_witness List.comparator_witness) Set.t
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

val info : Info.t -> t
val dot : label:string list -> attributes:string Core.String.Map.t -> t
val to_dot : t -> dot
val append : t -> t -> t
val to_string : ?shape:string -> name:string -> dot -> string
