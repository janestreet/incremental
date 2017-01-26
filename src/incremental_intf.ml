open Core

module type S = sig
  include Incremental_kernel.Incremental_intf.S_abstract_times with module Time := Time

  (** The weak versions of the memoization functions use a {!Weak_hashtbl} for the memo
      table.  This keeps a weak pointer to each result, and so the garbage collector
      automatically removes unused results.  Furthermore, [stabilize] removes the table
      entries whose result is unused.  *)

  val weak_memoize_fun
    :  ?initial_size : int  (** default is [4]. *)
    -> 'a Hashtbl.Hashable.t
    -> ('a -> 'b Heap_block.t)
    -> ('a -> 'b Heap_block.t) Staged.t

  val weak_memoize_fun_by_key
    :  ?initial_size : int  (** default is [4]. *)
    -> 'key Hashtbl.Hashable.t
    -> ('a -> 'key)
    -> ('a -> 'b Heap_block.t)
    -> ('a -> 'b Heap_block.t) Staged.t

end

module type Incremental = sig
  module Config = Incremental_kernel.Config
  module type Incremental_config = Config.Incremental_config
  module type S = S
  module Make                                      () : S
  module Make_with_config (C : Incremental_config) () : S
end
