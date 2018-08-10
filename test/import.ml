open! Core_kernel

include Expect_test_helpers_kernel

(* This removes [Balanced_reducer] from [Incremental.Private], because the type aliases in
   its interface means that [Incremental_debug] and [Incremental] disagree on it. *)
module type Incremental_intf_to_test_againt = sig
  include module type of Incremental
  with module Private := Incremental.Private
  module Private : sig
    val debug : bool
    val verbose : bool
  end
end
