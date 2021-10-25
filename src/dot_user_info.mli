(** A module internal to Incremental.  Users should see {!Incremental_intf}.

    A type describing user-controlled metadata optionally attached to nodes, usually for
    the purpose of debugging or inspecting the incremental graph. The core of incremental
    never uses this. *)

open Core
open! Import

module String_list : sig
  type t = string list

  include Comparator.S with type t := t
end

type dot =
  { label : Set.M(String_list).t
  ; attributes : string String.Map.t
  }
[@@deriving sexp_of]

type t =
  | Dot of dot
  | Info of Info.t
  | Append of
      { prior : t
      ; new_ : t
      }
[@@deriving sexp_of]

val info : Info.t -> t
val dot : label:string list -> attributes:string Core.String.Map.t -> t
val to_dot : t -> dot
val append : t -> t -> t
val to_string : name:string -> dot -> string
