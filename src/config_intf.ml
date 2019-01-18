open! Core_kernel
open! Import

module type Incremental_config = sig
  (** [bind_lhs_change_should_invalidate_rhs = false] is a hack to enable code that worked
      with earlier versions of Incremental that did not support invalidation to be more
      easily used with this version of Incremental.  Except in that situation, one
      should leave this as true, and that is what [Default] does. *)
  val bind_lhs_change_should_invalidate_rhs : bool
end

module type Config = sig
  module type Incremental_config = Incremental_config

  module Default () : Incremental_config
end
