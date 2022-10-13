# Incr_memoize

A library for memoizing incremental bind.

Overview
--------

Using `bind` in incremental code is often expensive - it causes the computation below the
bind to be discarded, and recomputed from scratch. Sometimes you can just use `map` instead
(see e.g. [incremental tutorial](../incremental/doc/part2-dynamic.mdx)
- `if_with_map` vs `if_with_bind`), but often you don't get the choice.

This can be tackled by keeping multiple computations, corresponding to different values of
nodes you bind to, and selecting the correct one below bind.

This library makes this super easy to do - by providing a Incr.bind replacement with
memoization super-powers. Basically, you can just replace

```ocaml
let%bind.Incr x = x in
following_computation x
```

with

```ocaml
module Incr_memoize = Incr_memoize.Make(Incr)

let%bind.Incr_memoize x = Incr_memoize.with_params x store_params in
following_computation x
```

It's almost drop-in, with the caveat that you need to provide some extra parameters for
configuring your memoization - stuff like a comparator, caching policy etc,
see `Incr_memoize.Store_params`.

See the tests for an example usage.

It is perfectly fine to call Incr_memoize.bind multiple times - bind to a few things one
after another, mix it with `Incr.bind`, `map`, use different `store_params` etc. Just keep in
mind, caching is most effective when you bind to things that only take few different values.
