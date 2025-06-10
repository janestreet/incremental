open! Core

module type S = sig @@ portable
  module Cutoff : sig
    type t =
      | Always
      | Never
      | Phys_equal
      | Compare
      | Equal
      | F
    [@@deriving sexp, equal]

    val to_string : t -> string
  end

  module Kind : sig
    type t =
      | Array_fold
      | At of { at : Time_ns.t }
      | At_intervals of
          { base : Time_ns.t
          ; interval : Time_ns.Span.t
          }
      | Bind_lhs_change
      | Bind_main
      | Const
      | Expert
      | Freeze
      | If_test_change
      | If_then_else
      | Invalid
      | Join_lhs_change
      | Join_main
      | Map
      | Snapshot of { at : Time_ns.t }
      | Step_function
      | Uninitialized
      | Unordered_array_fold
      | Var
      | Map2
      | Map3
      | Map4
      | Map5
      | Map6
      | Map7
      | Map8
      | Map9
      | Map10
      | Map11
      | Map12
      | Map13
      | Map14
      | Map15
    [@@deriving sexp]

    val to_string : t -> string
  end

  module Dot_user_info : sig
    type t [@@deriving sexp]

    type dot =
      { label : (string list, String.comparator_witness List.comparator_witness) Set.t
      ; attributes : string String.Map.t
      }
    [@@deriving sexp]

    val dot : label:string list -> attributes:string Core.String.Map.t -> t
    val to_dot : t -> dot
    val append : t -> t -> t
    val to_string : ?shape:string -> name:string -> dot -> string
    val default : name:string -> kind:Kind.t -> height:int -> t
  end

  module Stabilization_num : sig
    type t [@@deriving sexp]

    include Comparable.S with type t := t

    val to_string : t -> string
    val to_int : t -> int
    val is_some : t -> bool
    val is_none : t -> bool
  end

  module Node_id : sig
    type t [@@deriving sexp]

    include Hashable with type t := t
    include Comparable.S with type t := t

    val to_string : t -> string
    val to_int : t -> int
    val of_int : int -> t
  end

  type packed_node
  type _ state

  val node_id : packed_node -> Node_id.t
  val directly_observed : _ state -> packed_node list

  val traverse
    :  packed_node list
    -> add_node:
         (id:Node_id.t
          -> kind:Kind.t
          -> cutoff:Cutoff.t
          -> children:Node_id.t list
          -> bind_children:Node_id.t list
          -> user_info:Dot_user_info.t option
          -> recomputed_at:Stabilization_num.t
          -> changed_at:Stabilization_num.t
          -> height:int
          -> unit)
    -> unit
end
