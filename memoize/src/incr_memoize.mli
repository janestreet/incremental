(** A library for memoizing incremental bind. See README.md *)
open Core

module Store_params : sig
  (** Represents parameters to initialize a store that will cache incremental
      computations. *)

  type 'key t

  (** Use a map as a backend, never remove from cache *)
  val map_based__store_forever : ('key, _) Comparator.Module.t -> 'key t

  (** Don't memoize. Behaves identically to Incr.bind *)
  val none : 'key t

  (** Use a hash table as a backend, evict the computation for the ['key] that has been
      least recently accessed. *)
  val hash_based__lru
    :  max_size:int
    -> (module Hashtbl.Key_plain with type t = 'key)
    -> 'key t

  (** Use an alist as a backend, evict the computation for the ['key] that has been least
      recently accessed. Operations to access the memoized computation are O(max_size) on
      each change in ['key]. *)
  val alist_based__lru : equal:('key -> 'key -> bool) -> max_size:int -> 'key t

  (** Wraps another [_ t], and calls the provided functions every time a value is found in
      or added to the cache. You can use this to log or record statistics about how much
      benefit you're getting from memoization. *)
  val with_hooks : 'key t -> if_found:('key -> unit) -> if_added:('key -> unit) -> 'key t
end

module Store : sig
  type ('k, 'v) t

  val create : 'k Store_params.t -> ('k, 'v) t
  val find : ('k, 'v) t -> 'k -> 'v option
  val add : ('k, 'v) t -> key:'k -> value:'v -> unit
end

module Make (Incr : Incremental.S) : sig
  module Incr_with_store_params : sig
    type 'a t
  end

  val with_params : 'a Incr.t -> 'a Store_params.t -> 'a Incr_with_store_params.t

  (** Note that despite its appearance, this is not a true monad and does not satisfy the
      monad signature for [bind]. This exists to make [let%bind] syntax convenient. Be
      careful if you are trying to use this in an unconventional way. *)

  val bind : 'a Incr_with_store_params.t -> f:('a -> 'b Incr.t) -> 'b Incr.t
  val ( >>= ) : 'a Incr_with_store_params.t -> f:('a -> 'b Incr.t) -> 'b Incr.t

  module Let_syntax : sig
    module Let_syntax : sig
      val bind : 'a Incr_with_store_params.t -> f:('a -> 'b Incr.t) -> 'b Incr.t
    end
  end

  module Store_params = Store_params
  module Store = Store
end
