## Release v0.17.0
- Added `Scope.is_top` to determine if the scope is the top-level scope (outside of any
  binds)
- Add an optional `emit_bind_edges` parameter to all `save_dot` graphviz functions
  which you can use to omit the dashed lines connecting bind nodes to nodes allocated in
  their scope
- Expose `For_analyzer` module in the `Incremental` interface for traversal of `Incr`
  nodes for analysis purposes.

## Release v0.16.0

- Add `incr_memoize` library for memoizing incremental binds. This library keeps the
  incremental nodes of old binds alive by storing them in a data structure.
  - Introduce `Store_params` submodule for configuring the memoization behavior.

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## 113.43.00

- Adds a `Let_syntax` module to `Incremental_intf.S`. We've found things like this
  useful in a couple of different projects as a nice alternative to the `mapN`
  functions.

## 113.33.00

- Made it possible to use Incremental without invalidation -- i.e. a
  change in a the left-hand side of bind does *not* invalidate nodes
  created on the right-hand side.

  This is configurable via the argument to
  `Incremental.Make_with_config`, which now takes:

      val bind_lhs_change_should_invalidate_rhs : bool

  So, one can now build an Incremental without invalidation using:

      Incremental.Make_with_config (struct
        include Incremental.Config.Default ()
        let bind_lhs_change_should_invalidate_rhs = false
      end) ()

  Implementation
  --------------
  The implementation is simple:
    When a bind rhs changes, instead of `invalidate_nodes_created_on_rhs`,
    we `rescope_nodes_created_on_rhs`, which moves the nodes up to
    the bind's parent.

  Testing
  -------
  Turned the unit tests into a functor parameterized on
  `bind_lhs_change_should_invalidate_rhs`, and run them with both `true`
  and `false`.  Modified tests where necessary to skip tests of
  invalidity when `bind_lhs_change_should_invalidate_rhs = false`.

  Added a unit test of `bind_lhs_change_should_invalidate_rhs = true`
  that makes sure a node created on a bind rhs whose lhs subsequently
  changes continues to stabilize correctly.

- Splitted incremental into a part that can run in javascript, incremental_kernel, and the
  other one.

## 113.24.00

- Add README.org to Incremental.

- Added some type annotations based on comments by @def-lkb about lack of
  principality.

- Switched to ppx.
