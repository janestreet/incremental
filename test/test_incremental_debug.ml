(* This test is in a separate file from [test_incremental.ml] so that it runs in
   parallel with the analogous functor call at the bottom of that file. *)

include Test_incremental.Test (Incremental_debug)
