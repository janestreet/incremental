(** For expressing a computation that depends on variables and that can automatically
    incrementally recompute after the values of some of the variables change.

    {1 Incremental in a nutshell}

    Incremental is used to define a collection of interdependent values, some of which are
    "variables" set by user code and others that are defined via functions (in the
    mathematical and programming senses) of other incremental values.  Incremental
    automatically tracks all the dependencies between incremental values and can, on
    demand, propagate changed variables and recompute the incremental values that depend
    on them.

    To use incremental, one first creates a new instance via:

    {[
      module Inc : Incremental.S = Incremental.Make ()
    ]}

    The functor application creates data structures that will be shared throughout the
    lifetime of all incremental values used with this instance.  Since [Incremental.Make]
    is a generative functor, the type system enforces that different applications of the
    functor deal with disjoint sets of incrementals.

    For the remainder of this comment, we assume a particular [Inc] is [open]:

    {[
      open Inc
    ]}

    As an example of a simple computation, suppose we have integer variables [x] and [y]
    and want to keep an incremental value [z] defined by [z = x + y].  We could do this
    with:

    {[
      let x = Var.create 13
      let y = Var.create 17
      let z = map2 (Var.watch x) (Var.watch y) ~f:(fun x y -> x + y)
    ]}

    With this, as [x] and [y] change, incremental can recompute [z = x + y] on demand.
    Incremental only recomputes values that are being "observed", which one indicates by
    calling the [observe] function to get an "observer", e.g.:

    {[
      let z_o = observe z
    ]}

    Incremental doesn't compute [z] every time [x] and [y] change.  Rather, one must
    explicitly tell incremental when one wants [z] (and all other observed values) to be
    brought up to date, by calling [stabilize]:

    {[
      stabilize ();
    ]}

    At this point, the value of [z] is [30], which we can verify by:

    {[
      assert (Observer.value_exn z_o = 30);
    ]}

    If we change the value of [x] and then tell incremental to recompute observed values,
    then the value of [z] will change appropriately:

    {[
      Var.set x 19;
      stabilize ();
      assert (Observer.value_exn z_o = 36);
    ]}

    Another way to observe values is to use [Observer.on_update_exn], which attaches an
    "on-update handler" to an observer -- the handler will be run after each stabilization
    in which the observer's value changed (or was initialized) during stabilization.

    User functions given to incremental should never raise any exceptions: doing so will
    cause all future calls to [stabilize] to raise.

    {1 The incremental DAG}

    One can think of incrementals as forming a directed acyclic graph (DAG), where nodes
    correspond to incremental values and there is an edge from node [n1] to node [n2] if
    the value of [n2] depends on the value of [n1].  For example, the DAG for the above
    example has an edge from [x] to [z] and from [y] to [z].  The graph must be acyclic in
    order for the computation to be well defined.  The graph is a DAG rather than a tree
    because incremental values can be shared.  Extending the above example, we might have:

    {[
      let w = map2 (Var.watch y) z ~f:(fun y z -> y - z)
    ]}

    Both the node for [y] and the node for [z] are shared.

    We will use "node" to mean "incremental value" when we want to emphasize some aspect
    of the DAG.

    Say that a node is "observed" if there is an observer for it (created via [observe]).
    Say that a node is "necessary" if there is a path from that node to an observed node.
    [stabilize] ensures that all necessary nodes have correct values; it will not compute
    unnecessary nodes.  An unobserved node becomes necessary by a call to [observe] or by
    being used to compute an observed node; this will cause the appropriate DAG edges to
    be added.  A necessary node will become unnecessary if its observer (if any) becomes
    unused and if the node is no longer used to compute any observed nodes.  This will
    cause the appropriate DAG edges to be removed.

    Incremental does not know whether user-supplied functions (e.g. functions supplied to
    [bind] or [map]) are side effecting, and will not evaluate them for side effect.  If
    the resulting incrementals are not necessary then the function will not be called.

    {1 Stabilization}

    [stabilize] traverses the DAG in topological order starting at variables that changed
    since the last stabilization and recomputing their dependents.  This is done by using
    a "recompute heap" to visit the nodes in non-decreasing order of "height", which is a
    over-approximation of the longest path from a variable to that node.  To ensure that
    each node is computed at most once and that its children are stabilized before it is
    computed, nodes satisfy the property that if there is an edge from n1 to n2, then the
    height of n1 is less than the height of n2.

    [stabilize] repeats the following steps until the heap becomes empty:

    1. remove from the recompute heap a node with the smallest height
    2. recompute that node
    3. if the node's value changes, then add its parents to the heap.

    The definition of "changes" in step (3) is configurable by user code.  By default, a
    node is considered to change if its new value is not [phys_equal] to the previous
    value.  One can use [set_cutoff] on a node to change its cutoff function, e.g. for
    [floats] one could cutoff propagation if the old value and new value are closer than
    some threshold.

    If [stabilize] ever raises due to an error, then the incremental system becomes
    unusable, and all future calls to [stabilize] will immediately raise.

    Stabilization uses a heap implemented with an array whose length is the max height, so
    for good performance, the height of nodes must be small.  There is an upper bound on
    the height of nodes, [max_height_allowed], which defaults to 128.  An attempt to
    create a node with larger height will raise.  One can dynamically increase
    [max_height_allowed]; however, one should be wary of doing so, for performance
    reasons.

    {1 Bind}

    Much of the power of incremental comes from [bind], also written [>>=].  As a
    reminder, [bind] has this type:

    {[
      val bind : 'a t -> f:('a -> 'b t) -> 'b t
    ]}

    [bind ta ~f] returns an incremental [tb] that behaves like [f a], where [a] is the
    most recent value of [ta].  The implementation only calls [f] when the value of [ta]
    changes.  Thinking in terms of the DAG, [bind ta ~f] returns a node [tb] such that
    whenever the value of [ta] changes, the implementation calls [f] to obtain a node
    (possibly with an arbitrary DAG below it) that defines the value of [tb].

    [bind] can be used to transition existing parts of the graph between necessary and
    unnecessary.  E.g.:

    {[
      val if_ : bool t -> a t -> a t -> a t
      let if_ a b c = bind a ~f:(fun a -> if a then b else c)
    ]}

    With [let t = if_ a b c], when [a] is [true], if [t] is necessary, then [b] will be
    necessary, but [c] will not.  And vice-versa when [a] is [false].

    Even more, [bind] allows one to dynamically create an arbitrary graph based on the
    value of some other incremental, and to "hide" that dynamism behind an ordinary
    incremental value.  One common way to use this is for dynamic reconfiguration, e.g.:

    {[
      let config_var = Var.create config in
      bind (Var.watch config_var) ~f:(fun config -> ... )
    ]}

    Then, whenever one wants to reconfigure the system, one does [Var.set config_var]
    and then [stabilize], which will construct a new DAG according to the new config.

    Bind nodes introduce special height constraints, so that stabilization is guaranteed
    to recompute the left-hand side of a bind before recomputing any node created by the
    right-hand side [f].  This avoids recomputing nodes created on the right-hand side
    that would then become unnecessary when the left-hand side changes.  More precisely,
    in [t >>= f], any node created by [f] is made to have a height larger than [t].  This
    rule applies also to bind nodes created by [f], so that ultimately the height of every
    node is greater than the height of all the left-hand sides of the binds that were
    involved in its creation.  The height requirement does not apply to nodes returned by
    [f] but not created by [f] -- such nodes depend on the bind in effect when they were
    created, but have no dependence on [t].

    When the left-hand side of a bind node changes, stabilization "invalidates" all the
    nodes that depend on it (because they may use an old value of the left-hand side).

    For example, consider:

    {[
      let t1 = map ... in
      bind t2 ~f:(fun _ ->
        let t3 = map ... in
        map2 t1 t3 ~f:(...))
    ]}

    In this example, [t1] is created outside of [bind t2], whereas [t3] is created by the
    right-hand side of [bind t2].  So, [t3] depends on [t2] (and has a greater height),
    whereas [t1] does not.  And, in a stabilization in which [t2] changes, we are
    guaranteed to not recompute the old [t3], but we have no such guarantee about [t1].
    Furthermore, when [t2] changes, the old [t3] will be invalidated, whereas [t1] will
    not.

    Since [bind] essentially allows one to add arbitrary edges to the DAG, one can use it
    to construct a cycle.  [stabilize] will detect such cycles and raise.

    {1 Garbage collection}

    Incremental maintains three kinds of pointers:

    - from nodes to their children (nodes they depend on).
    - from necessary nodes to their necessary parents (nodes that depend on them).
    - from observers to the nodes they observe.

    So, all necessary nodes are kept alive, from the perspective of the garbage collector.

    If an observer has no on-update handlers and user code no longer holds on to it,
    incremental (via a finalizer on the observer), detects this and disallows future use
    of the observer, making the node it observed unnecessary if it is not necessary for
    another reason.  One can eagerly remove an observer by calling [disallow_future_use].
    Because finalizers may be called much later than when an observer actually becomes
    unreachable, it is preferable to disable observers using [disallow_future_use] to
    avoid useless propagation during stabilizations.

    If an observer has on-update handlers, calling [disallow_future_use] is the only way
    to have it removed.

    {1 The implementation}

    The key type in the implementation is [Node.t], which represents one node in the
    incremental DAG.  The node type is in fact the same as [Incremental.t], although this
    type equivalence is not exposed.  A node is a record with many fields (> 20).  In
    particular a node holds:

    - kind -- the kind of node it is (const, var, map, bind, snapshot, etc.).
    - value -- the node's current value.
    - recompute id -- the stabilization number at which it was last recomputed.
    - change id -- the stabilization number at which its value last changed.
    - height -- the height of the node in the DAG.
    - parents -- the necessary nodes that depend on it.
    - children -- the nodes that it depends on.
    - created_in -- the scope in which it was created.

    Say that a node is "stale" if it has never been computed or if its recompute id is
    less than the change id of one of its children.  A node should be recomputed if it
    is both necessary and stale.

    The [State.t] type holds all the mutable data used to implement stabilization.  In
    particular, the incremental state contains:

    - the current stabilization number
    - the set of observers
    - a recompute heap -- nodes that need to be recomputed, ordered by height.
    - an adjust-heights heap -- nodes whose height needs increasing, ordered by height.

    The goals of stabilization are to:

    - compute all necessary nodes that need to be recomputed.
    - only compute necessary nodes.
    - compute each node at most once.
    - only compute a node after ensuring its children are up to date.

    To do this, incremental maintains the following invariants:

    - [p] is in [c]'s parents iff ([c] is in [p]'s children && [p] is necessary)
    - [p] is in the recompute heap iff [p] is necessary and stale.
    - if [p] is a parent of [c], then [p]'s height is greater than [c]'s height.

    The first invariant ensures that when a node's value changes, we can reach from it all
    necessary nodes (and only the necessary nodes) that depend on it.  The second
    invariant ensures that that stabilization only computes necessary nodes.  The third
    invariant, combined with the fact that stabilization always recomputes a node from the
    recompute-heap that has minimum height, ensures that we only compute a node after all
    its children are stable, and that we compute each node at most once.

    Finally, at the end of stabilization, the recompute heap is empty, so the invariant
    implies that there are no necessary nodes that are stale, i.e. stabilization has
    computed all necessary nodes that need to be recomputed.

    {1 Maintaining the parent invariant}

    Maintaining the invariant that a node has edges only to necessary parents requires
    traversing a node's descendants when it transitions between necessary and unnecessary,
    in order to add or remove parents as appropriate.  For example, when an observer is
    first added to an unnecessary node, the implementation visits all its descendants to
    add parents.  This is essentially a form of ref-counting, in which the counter is the
    number of parents that a node has.  There is no problem with cycles because the DAG
    requirement on the graph is enforced.

    {1 Maintaining the height invariant and checking for cycles}

    Maintaining the invariant that a necessary node's height is larger than all of its
    children requires adjusting heights when an edge is added to the DAG (e.g. when a bind
    left-hand side changes).  This is done using the "adjust-heights" heap.  When an edge
    is added, if the child's height is greater than or equal to the parent's height, then
    the adjust-heights heap increases the height of the parent and all of the parent's
    ancestors as necessary in order to restore the height invariant.  This is done by
    visiting ancestors in topological order, in increasing order of pre-adjusted height.
    If during that traversal, the child of the original edge is visited, then there is a
    cycle in the graph, and stabilization raises.

    In pathological situations, the implementation will raise due to a cyclic graph even
    though subsequent graph operations would eliminate the cycle.  This is because the
    cyclicity check happens after each edge is added, rather than waiting until a batch
    of graph changes.

    {1 Bind, scopes, and invalidation}

    Much of the complexity of the implementation comes from [bind].  In [t >>= f], when
    [f] is applied to the value of [t], all of the nodes that are created depend on that
    value.  If the value of [t] changes, then those nodes no longer make sense because
    they depend on a stale value.  It would be both wasteful and wrong to recompute any of
    those "invalid" nodes.  So, the implementation maintains the invariant that the height
    of a necessary node is greater than the height of the left-hand side of the nearest
    enclosing bind.  That guarantees that stabilization will stabilize the left-hand side
    before recomputing any nodes created on the right-hand side.  Furthermore, if the
    left-hand side's value changes, stabilization marks all the nodes on the right-hand
    side as invalid.  Such invalid nodes will typically be unnecessary, but there are
    pathological cases where they remain necessary.

    The bind height invariant is accomplished using a special "bind-lhs-change" node,
    which is a parent of the bind-lhs and a child of the bind result.  The incremental
    state maintains the "current scope", which is the bind whose right-hand side is
    currently being evaluated, or a special "top" scope if there is no bind in effect.
    Each node has a [created_in] field set to the scope in effect when the node is
    created.  The implementation keeps for each scope, a singly-linked list of all nodes
    created in that scope.  Invalidation traverses this list, and recurs on bind nodes in
    it to traverse their scopes as well.

    [if_] and [join] are special cases of [bind] that manipulate the graph; however they
    do not create new scopes.  They use a similar lhs-change node to detect changes and
    perform graph manipulation.

    {1 Debugging}

    For performance reasons, [Incremental] is built with debugging asserts disabled.
    [Incremental_debug] is a library that uses the same code as [Incremental], but has
    debugging asserts enabled (via an [IFDEF]).  [Incremental_debug] is significantly
    slower than [Incremental], but may detect a bug in the Incremental library that would
    otherwise remain undetected by [Incremental].

    {1 Reading guide}

    Here's a breakdown of the modules in roughly dependency order.

    {ul
    {li [Import] -- imports from other libraries, and commonly used functions }
    {li Basic types.
    - [Cutoff] -- a cutoff function
    - [On_update_handler] -- a function to run when a node's value changes
    - [Node_id] -- an integer unique id for nodes
    - [Raised_exn] -- a wrapper around [exn] that keeps a backtrace.
    - [Sexp_of] -- interfaces for types that have [with sexp_of].
    - [Stabilization_num] -- an abstract [int option], used to express the stabilization
      cycle when something happens. }
      {li [Types] -- mutually recursive types.
      Many of the types used in the implementation are mutually recursive.  They are
      all defined in [Types].  Each type is then later defined in its own module, along
      with [with fields, sexp].  }
      {li [Kind] -- the variant with one constructor for each kind of node, plus a special
      constructor for invalidated nodes.  Many of the value-carrying variants also have a
      module for its argument type:
    - [Array_fold]
    - [At]
    - [At_intervals]
    - [Bind]
    - [Freeze]
    - [If_then_else]
    - [Join]
    - [Snapshot]
    - [Step_function_node]
    - [Unordered_array_fold]
    - [Var]  }
      {li [Scope] -- a packed bind. }
      {li [Node] -- the main node type. }
      {li [Internal_observer] }
      {li [Observer] -- a [ref] wrapper around [Internal_observer], used so a finalizer
      can detect when user code is done with an observer. }
      {li [Recompute_heap] }
      {li [Adjust_heights_heap] }
      {li [Alarm_value] -- values stored in the timing wheel, for time-based nodes. }
      {li [State] -- the record type will all data structures used for stabilization, and
      the implementation of all the [Incremental] functions. }
      {li [Incremental], the main functor, mostly a wrapper around [State]. }
      {li [Incremental_unit_tests]. }
      } *)

open Core_kernel
open! Import

module type Map_n_gen = sig
  type ('a, 'w) t

  val map2 : ('a1, 'w) t -> ('a2, 'w) t -> f:('a1 -> 'a2 -> 'b) -> ('b, 'w) t

  val map3
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'b)
    -> ('b, 'w) t

  val map4
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b)
    -> ('b, 'w) t

  val map5
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b)
    -> ('b, 'w) t

  val map6
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b)
    -> ('b, 'w) t

  val map7
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b)
    -> ('b, 'w) t

  val map8
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'b)
    -> ('b, 'w) t

  val map9
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'b)
    -> ('b, 'w) t

  val map10
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> ('a10, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a10 -> 'b)
    -> ('b, 'w) t

  val map11
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> ('a10, 'w) t
    -> ('a11, 'w) t
    -> f:
         ('a1
          -> 'a2
          -> 'a3
          -> 'a4
          -> 'a5
          -> 'a6
          -> 'a7
          -> 'a8
          -> 'a9
          -> 'a10
          -> 'a11
          -> 'b)
    -> ('b, 'w) t

  val map12
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> ('a10, 'w) t
    -> ('a11, 'w) t
    -> ('a12, 'w) t
    -> f:
         ('a1
          -> 'a2
          -> 'a3
          -> 'a4
          -> 'a5
          -> 'a6
          -> 'a7
          -> 'a8
          -> 'a9
          -> 'a10
          -> 'a11
          -> 'a12
          -> 'b)
    -> ('b, 'w) t

  val map13
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> ('a10, 'w) t
    -> ('a11, 'w) t
    -> ('a12, 'w) t
    -> ('a13, 'w) t
    -> f:
         ('a1
          -> 'a2
          -> 'a3
          -> 'a4
          -> 'a5
          -> 'a6
          -> 'a7
          -> 'a8
          -> 'a9
          -> 'a10
          -> 'a11
          -> 'a12
          -> 'a13
          -> 'b)
    -> ('b, 'w) t

  val map14
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> ('a10, 'w) t
    -> ('a11, 'w) t
    -> ('a12, 'w) t
    -> ('a13, 'w) t
    -> ('a14, 'w) t
    -> f:
         ('a1
          -> 'a2
          -> 'a3
          -> 'a4
          -> 'a5
          -> 'a6
          -> 'a7
          -> 'a8
          -> 'a9
          -> 'a10
          -> 'a11
          -> 'a12
          -> 'a13
          -> 'a14
          -> 'b)
    -> ('b, 'w) t

  val map15
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> ('a5, 'w) t
    -> ('a6, 'w) t
    -> ('a7, 'w) t
    -> ('a8, 'w) t
    -> ('a9, 'w) t
    -> ('a10, 'w) t
    -> ('a11, 'w) t
    -> ('a12, 'w) t
    -> ('a13, 'w) t
    -> ('a14, 'w) t
    -> ('a15, 'w) t
    -> f:
         ('a1
          -> 'a2
          -> 'a3
          -> 'a4
          -> 'a5
          -> 'a6
          -> 'a7
          -> 'a8
          -> 'a9
          -> 'a10
          -> 'a11
          -> 'a12
          -> 'a13
          -> 'a14
          -> 'a15
          -> 'b)
    -> ('b, 'w) t
end

module type Map_n = sig
  type 'a t

  include Map_n_gen with type ('a, 'w) t := 'a t
end

module type Bind_n_gen = sig
  type ('a, 'w) t

  val bind2 : ('a1, 'w) t -> ('a2, 'w) t -> f:('a1 -> 'a2 -> ('b, 'w) t) -> ('b, 'w) t

  val bind3
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> ('b, 'w) t)
    -> ('b, 'w) t

  val bind4
    :  ('a1, 'w) t
    -> ('a2, 'w) t
    -> ('a3, 'w) t
    -> ('a4, 'w) t
    -> f:('a1 -> 'a2 -> 'a3 -> 'a4 -> ('b, 'w) t)
    -> ('b, 'w) t
end

module type Bind_n = sig
  type 'a t

  include Bind_n_gen with type ('a, 'w) t := 'a t
end

(** [S_gen] is the type of the module returned by [Incremental.Make].  It is a
    specialization of the interface of [Incremental], with:

    - the ['w] state_witness type parameter removed
    - the [State.t] argument removed

    The comments for components of [S_gen] are in [module type Incremental] below. *)
module type S_gen = sig
  module State : sig
    type t [@@deriving sexp_of]

    include Invariant.S with type t := t

    (** [t] is the shared state for a call to [Incremental.Make]. *)
    val t : t

    val keep_node_creation_backtrace : t -> bool
    val set_keep_node_creation_backtrace : t -> bool -> unit
    val max_height_allowed : t -> int
    val set_max_height_allowed : t -> int -> unit
    val num_active_observers : t -> int
    val max_height_seen : t -> int
    val num_nodes_became_necessary : t -> int
    val num_nodes_became_unnecessary : t -> int
    val num_nodes_changed : t -> int
    val num_nodes_created : t -> int
    val num_nodes_invalidated : t -> int
    val num_nodes_recomputed : t -> int
    val num_nodes_recomputed_directly_because_one_child : t -> int
    val num_nodes_recomputed_directly_because_min_height : t -> int
    val num_stabilizes : t -> int
    val num_var_sets : t -> int

    module Stats : sig
      type t [@@deriving sexp_of]
    end

    val stats : t -> Stats.t
  end

  type 'a t [@@deriving sexp_of]
  type 'a incremental := 'a t

  include Invariant.S1 with type 'a t := 'a t

  val is_const : _ t -> bool
  val is_valid : _ t -> bool
  val is_necessary : _ t -> bool
  val const : 'a -> 'a t
  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  include Map_n with type 'a t := 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  include Bind_n with type 'a t := 'a t

  module Infix : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val join : 'a t t -> 'a t
  val if_ : bool t -> then_:'a t -> else_:'a t -> 'a t
  val freeze : ?when_:('a -> bool) -> 'a t -> 'a t
  val depend_on : 'a t -> depend_on:_ t -> 'a t
  val necessary_if_alive : 'a t -> 'a t
  val for_all : bool t array -> bool t
  val exists : bool t array -> bool t
  val all : 'a t list -> 'a list t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val array_fold : 'a t array -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

  val reduce_balanced
    :  'a t array
    -> f:('a -> 'b)
    -> reduce:('b -> 'b -> 'b)
    -> 'b t option

  module Unordered_array_fold_update : sig
    type ('a, 'b) t =
      | F_inverse of ('b -> 'a -> 'b)
      | Update of ('b -> old_value:'a -> new_value:'a -> 'b)
  end

  val unordered_array_fold
    :  ?full_compute_every_n_changes:int
    -> 'a t array
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> update:('a, 'b) Unordered_array_fold_update.t
    -> 'b t

  val opt_unordered_array_fold
    :  ?full_compute_every_n_changes:int
    -> 'a option t array
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> f_inverse:('b -> 'a -> 'b)
    -> 'b option t

  val sum
    :  ?full_compute_every_n_changes:int
    -> 'a t array
    -> zero:'a
    -> add:('a -> 'a -> 'a)
    -> sub:('a -> 'a -> 'a)
    -> 'a t

  val opt_sum
    :  ?full_compute_every_n_changes:int
    -> 'a option t array
    -> zero:'a
    -> add:('a -> 'a -> 'a)
    -> sub:('a -> 'a -> 'a)
    -> 'a option t

  val sum_int : int t array -> int t
  val sum_float : float t array -> float t

  module Scope : sig
    type t

    val top : t
    val current : unit -> t
    val within : t -> f:(unit -> 'a) -> 'a
  end

  module Var : sig
    type 'a t [@@deriving sexp_of]

    val create : ?use_current_scope:bool -> 'a -> 'a t
    val set : 'a t -> 'a -> unit
    val watch : 'a t -> 'a incremental
    val value : 'a t -> 'a
    val latest_value : 'a t -> 'a
    val replace : 'a t -> f:('a -> 'a) -> unit
  end

  module Observer : sig
    type 'a t [@@deriving sexp_of]

    include Invariant.S1 with type 'a t := 'a t

    val observing : 'a t -> 'a incremental
    val use_is_allowed : _ t -> bool
    val value : 'a t -> 'a Or_error.t
    val value_exn : 'a t -> 'a

    module Update : sig
      type 'a t =
        | Initialized of 'a
        | Changed of 'a * 'a
        | Invalidated
      [@@deriving compare, sexp_of]
    end

    val on_update_exn : 'a t -> f:('a Update.t -> unit) -> unit
    val disallow_future_use : _ t -> unit
  end

  val observe : ?should_finalize:bool -> 'a t -> 'a Observer.t

  module Update : sig
    type 'a t =
      | Necessary of 'a
      | Changed of 'a * 'a
      | Invalidated
      | Unnecessary
    [@@deriving compare, sexp_of]
  end

  val on_update : 'a t -> f:('a Update.t -> unit) -> unit
  val stabilize : unit -> unit
  val am_stabilizing : unit -> bool

  module Cutoff : sig
    type 'a t [@@deriving sexp_of]

    include Invariant.S1 with type 'a t := 'a t

    val create : (old_value:'a -> new_value:'a -> bool) -> 'a t
    val of_compare : ('a -> 'a -> int) -> 'a t
    val of_equal : ('a -> 'a -> bool) -> 'a t
    val always : _ t
    val never : _ t
    val phys_equal : _ t
    val poly_equal : _ t
    val should_cutoff : 'a t -> old_value:'a -> new_value:'a -> bool
    val equal : 'a t -> 'a t -> bool
  end

  val set_cutoff : 'a t -> 'a Cutoff.t -> unit
  val get_cutoff : 'a t -> 'a Cutoff.t
  val lazy_from_fun : (unit -> 'a) -> 'a Lazy.t
  val default_hash_table_initial_size : int

  val memoize_fun
    :  ?initial_size:int
    -> 'a Base.Hashtbl.Key.t
    -> ('a -> 'b)
    -> ('a -> 'b) Staged.t

  val memoize_fun_by_key
    :  ?initial_size:int
    -> 'key Base.Hashtbl.Key.t
    -> ('a -> 'key)
    -> ('a -> 'b)
    -> ('a -> 'b) Staged.t

  val weak_memoize_fun
    :  ?initial_size:int
    -> 'a Base.Hashtbl.Key.t
    -> ('a -> 'b Heap_block.t)
    -> ('a -> 'b Heap_block.t) Staged.t

  val weak_memoize_fun_by_key
    :  ?initial_size:int
    -> 'key Base.Hashtbl.Key.t
    -> ('a -> 'key)
    -> ('a -> 'b Heap_block.t)
    -> ('a -> 'b Heap_block.t) Staged.t

  val user_info : _ t -> Info.t option
  val set_user_info : _ t -> Info.t option -> unit

  module Node_value : sig
    type 'a t =
      | Invalid
      | Necessary_maybe_stale of 'a option
      | Unnecessary_maybe_stale of 'a option
    [@@deriving sexp_of]
  end

  (** [node_value t] returns whatever value [t] happens to have in it, regardless of
      whether [t] is valid, necessary, or stale.  One should use [observe] for a more
      sensible semantics, reserving [node_value] for debugging. *)
  val node_value : 'a t -> 'a Node_value.t

  module Packed : sig
    type t

    val save_dot : string -> t list -> unit
  end

  val pack : _ t -> Packed.t
  val save_dot : string -> unit

  module Let_syntax : sig
    val return : 'a -> 'a t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    module Let_syntax : sig
      val bind : 'a t -> f:('a -> 'b t) -> 'b t

      include Bind_n with type 'a t := 'a t

      val map : 'a t -> f:('a -> 'b) -> 'b t

      include Map_n with type 'a t := 'a t

      val both : 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs : sig
        val watch : 'a Var.t -> 'a t
      end
    end
  end

  module Before_or_after : sig
    type t =
      | Before
      | After
    [@@deriving sexp_of]
  end

  module Step_function = Step_function

  module Clock : sig
    type t [@@deriving sexp_of]

    val default_timing_wheel_config : Timing_wheel.Config.t

    val create
      :  ?timing_wheel_config:Timing_wheel.Config.t
      -> start:Time_ns.t
      -> unit
      -> t

    val alarm_precision : t -> Time_ns.Span.t
    val timing_wheel_length : t -> int
    val now : t -> Time_ns.t
    val watch_now : t -> Time_ns.t incremental
    val advance_clock : t -> to_:Time_ns.t -> unit
    val advance_clock_by : t -> Time_ns.Span.t -> unit
    val at : t -> Time_ns.t -> Before_or_after.t incremental
    val after : t -> Time_ns.Span.t -> Before_or_after.t incremental
    val at_intervals : t -> Time_ns.Span.t -> unit incremental
    val step_function : t -> init:'a -> (Time_ns.t * 'a) list -> 'a incremental
    val incremental_step_function : t -> 'a Step_function.t incremental -> 'a incremental

    val snapshot
      :  t
      -> 'a incremental
      -> at:Time_ns.t
      -> before:'a
      -> 'a incremental Or_error.t
  end

  module Expert : sig
    module Dependency : sig
      type 'a t [@@deriving sexp_of]

      val create : ?on_change:('a -> unit) -> 'a incremental -> 'a t
      val value : 'a t -> 'a
    end

    module Node : sig
      type 'a t [@@deriving sexp_of]

      val create
        :  ?on_observability_change:(is_now_observable:bool -> unit)
        -> (unit -> 'a)
        -> 'a t

      val watch : 'a t -> 'a incremental
      val make_stale : _ t -> unit
      val invalidate : _ t -> unit
      val add_dependency : _ t -> _ Dependency.t -> unit
      val remove_dependency : _ t -> _ Dependency.t -> unit
    end
  end
end

module type Incremental = sig
  (** A [State.t] holds shared state used by all the incremental functions. *)
  module State : sig
    type 'w t [@@deriving sexp_of]

    module type S = sig
      type state_witness [@@deriving sexp_of]

      val t : state_witness t
    end

    val create : ?max_height_allowed:int (** default is 128 *) -> unit -> (module S)

    (** If [keep_node_creation_backtrace], then whenever a new node is created,
        incremental will call [Backtrace.get] and store the result in the node.  The
        backtrace will then appear in subsequent error messages when the node is pretty
        printed. *)
    val keep_node_creation_backtrace : _ t -> bool

    val set_keep_node_creation_backtrace : _ t -> bool -> unit
    val max_height_allowed : _ t -> int

    (** [set_max_height_allowed t height] sets the maximum allowed height of nodes.
        [set_max_height_allowed] raises if called during stabilization, or if [height <
        max_height_seen t]. *)
    val set_max_height_allowed : _ t -> int -> unit

    (** [num_active_observers] returns (in constant time) the number of observers that
        have been created and not yet disallowed (either explicitly or via
        finalization). *)
    val num_active_observers : _ t -> int

    (** {2 constant-time stats} These are counters that are constant time to read, and
        that are automatically updated in the ordinary course.  *)

    val max_height_seen : _ t -> int
    val num_nodes_became_necessary : _ t -> int
    val num_nodes_became_unnecessary : _ t -> int

    (** Number of times a node has seen its value changed, the determination of which
        depends on the choice of cutoff. *)
    val num_nodes_changed : _ t -> int

    val num_nodes_created : _ t -> int
    val num_nodes_invalidated : _ t -> int
    val num_nodes_recomputed : _ t -> int
    val num_nodes_recomputed_directly_because_one_child : _ t -> int
    val num_nodes_recomputed_directly_because_min_height : _ t -> int
    val num_stabilizes : _ t -> int
    val num_var_sets : _ t -> int

    (** [Stats] contains information about the DAG intended for human consumption.

        [stats] takes time proportional to the number of necessary nodes. *)
    module Stats : sig
      type t [@@deriving sexp_of]
    end

    val stats : _ t -> Stats.t
  end

  (** [type ('a,'w) t] is the type of incrementals that have a value of type ['a], with a
      state witness of type ['w].

      Incrementals are not covariant, i.e. we do not have [(+'a, _) t] -- consider,
      e.g. [set_cutoff] and [get_cutoff].  However, if you have types [a1] and [a2] where
      [a1] is a subtype of [a2], and a value [t1 : a1 t], then the following builds an
      incremental value of type [a2 t]:

      {[
        let t2 : a2 t = t1 >>| fun a1 -> (a1 : a1 :> a2)
      ]} *)
  type ('a, 'w) t [@@deriving sexp_of]

  type ('a, 'w) incremental := ('a, 'w) t

  include Invariant.S2 with type ('a, 'w) t := ('a, 'w) t

  val state : (_, 'w) t -> 'w State.t

  (** If [is_const t] then [t] is a constant-valued incremental.  [is_const (const a)] is
      true. *)
  val is_const : _ t -> bool

  val is_valid : _ t -> bool
  val is_necessary : _ t -> bool

  (** {1 Creating incrementals} *)

  (** [const state a] returns an incremental whose value never changes.  It is the same as
      [return], but reads more clearly in many situations because it serves as a nice
      reminder that the incremental won't change (except possibly be invalidated). *)
  val const : 'w State.t -> 'a -> ('a, 'w) t

  val return : 'w State.t -> 'a -> ('a, 'w) t

  (** [map t1 ~f] returns an incremental [t] that maintains its value as [f a], where [a]
      is the value of [t1].  [map2], [map3], ..., [map9] are the generalizations to more
      arguments.  If you need map<N> for some N > 9, it can easily be added, but also see
      [array_fold] and [unordered_array_fold].

      [f] should not create incremental nodes but this behavior is not checked; if you
      want to create incremental nodes, use [bind].  The invalidation machinery that is
      used with [bind] is not used with [map]. *)
  val map : ('a, 'w) t -> f:('a -> 'b) -> ('b, 'w) t

  val ( >>| ) : ('a, 'w) t -> ('a -> 'b) -> ('b, 'w) t

  include Map_n_gen with type ('a, 'w) t := ('a, 'w) t

  (** [bind t1 ~f] returns an incremental [t2] that behaves like [f v], where [v] is the
      value of [t1].  If [t1]'s value changes, then incremental applies [f] to that new
      value and [t2] behaves like the resulting incremental.

      [bind] can be significantly more expensive than [map] during stabilization, because,
      when its left-hand side changes, it requires modification of the incremental DAG,
      while [map] simply flows values along the DAG.  Thus it is preferable to use [map]
      (and its n-ary variants above) instead of [bind] unless one actually needs [bind]'s
      power.

      [bind2 t1 t2 ~f] is:

      {[
        bind (map2 t1 t2 ~f:(fun v1 v2 -> (v1, v2)))
          ~f:(fun (v1, v2) -> f v1 v2)
      ]}

      This is equivalent to [bind t1 ~f:(fun v1 -> bind t2 ~f:(fun v2 -> f v1 v2))] but
      more efficient due to using one bind node rather than two.  The other [bind<N>]
      functions are generalize to more arguments. *)
  val bind : ('a, 'w) t -> f:('a -> ('b, 'w) t) -> ('b, 'w) t

  val ( >>= ) : ('a, 'w) t -> ('a -> ('b, 'w) t) -> ('b, 'w) t

  include Bind_n_gen with type ('a, 'w) t := ('a, 'w) t

  module Infix : sig
    val ( >>| ) : ('a, 'w) t -> ('a -> 'b) -> ('b, 'w) t
    val ( >>= ) : ('a, 'w) t -> ('a -> ('b, 'w) t) -> ('b, 'w) t
  end

  (** [join t] returns an incremental that behaves like the incremental that [t] currently
      holds. *)
  val join : (('a, 'w) t, 'w) t -> ('a, 'w) t

  (** [if_ tb ~then_ ~else_] returns an incremental [t] that holds the value of [then_] if
      [tb] is true, the value of [else_] if [tb] is false.  Note that [t] only depends on
      one of [then_] or [else_] at a time, i.e. [if_ tb ~then_ ~else] is like:

      {[
        bind b ~f:(fun b -> if b then then_ else else_)
      ]}

      which is not the same as:

      {[
        map3 b then_ else_ ~f:(fun b then_ else_ -> if b then then_ else else_)
      ]} *)
  val if_ : (bool, 'w) t -> then_:('a, 'w) t -> else_:('a, 'w) t -> ('a, 'w) t

  (** [freeze ?when_ t] returns an incremental whose value is [t]'s value [v] until the
      first stabilization in which [when_ v] holds, at which point the freeze node's value
      becomes constant and never changes again.  Calling [freeze t] forces [t] to be
      necessary until it freezes regardless of whether the freeze node is necessary, but
      not thereafter (although of course [t] could remain necessary for other reasons).
      The result of [freeze t], once frozen, will never be invalidated, even if [t] is
      invalidated, and even if the scope in which the freeze is created is invalidated.
      However, prior to [when_ v] becoming true, [freeze t] can be invalidated. *)
  val freeze : ?when_:('a -> bool) -> ('a, 'w) t -> ('a, 'w) t

  (** [depend_on input ~depend_on] returns an [output] whose value is the same as
      [input]'s value, such that [depend_on] is necessary so long as [output] is
      necessary.  It is like:

      {[
        map2 input depend_on ~f:(fun a _ -> a)
      ]}

      but with a cutoff such that [output]'s value only changes when [input]'s value
      changes. *)
  val depend_on : ('a, 'w) t -> depend_on:(_, 'w) t -> ('a, 'w) t

  (** [necessary_if_alive input] returns [output] that has the same value and cutoff as
      [input], such that as long as [output] is alive, [input] is necessary. *)
  val necessary_if_alive : ('a, 'w) t -> ('a, 'w) t

  (** [for_all ts] returns an incremental that is [true] iff all [ts] are [true]. *)
  val for_all : 'w State.t -> (bool, 'w) t array -> (bool, 'w) t

  (** [exists ts] returns an incremental that is [true] iff at least one of the [ts] is
      [true]. *)
  val exists : 'w State.t -> (bool, 'w) t array -> (bool, 'w) t

  (** [all ts] returns an incremental whose value is a list of the values of all of the
      [ts].  In any stabilization where any of the [ts] changes, the entire list is
      recreated (once all of the [ts] have stabilized).  This essentially an [array_fold]
      over the [ts]. *)
  val all : 'w State.t -> ('a, 'w) t list -> ('a list, 'w) t

  (** [both t1 t2] returns an incremental whose value is pair of values of [t1] and [t2].
      Both [map (both t1 t2) ~f] and [map2 t1 t2 ~f:(fun a1 a2 -> f (a1, a2))] return an
      incremental with the same behavior, but the [map2] version is more efficient,
      because it creates a single node, whereas the [both] version creates two nodes. *)
  val both : ('a, 'w) t -> ('b, 'w) t -> ('a * 'b, 'w) t

  (** {1 Array folds and sums} *)

  (** [array_fold ts ~init ~f] creates an incremental [t] whose value is:

      {[
        Array.fold ts ~init ~f:(fun ac t -> f ac (value t))
      ]}

      In a stabilization during which any of the [ts] changes, the entire fold will be
      computed once all of the [ts] have been computed. *)
  val array_fold
    :  'w State.t
    -> ('a, 'w) t array
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> ('b, 'w) t

  (** [reduce_balanced ts ~f ~reduce] does a fold-like operation over [ts].  Unlike
      [array_fold], the operation will be computed in [O(min(n, k * log(n)))] time, where
      [n] is the size of [ts] and [k] is the number of elements of [ts] that have changed
      since the last stabilization.

      Generally, if most or all of [ts] are changing between stabilizations, using
      [array_fold] will have better constant factors.

      The [reduce] argument must be an associative operation:
      [reduce a (reduce b c) = (reduce (reduce a b) c)].

      [None] is returned upon supplying an empty array. *)
  val reduce_balanced
    :  'w State.t
    -> ('a, 'w) t array
    -> f:('a -> 'b)
    -> reduce:('b -> 'b -> 'b)
    -> ('b, 'w) t option

  module Unordered_array_fold_update : sig
    type ('a, 'b) t =
      | F_inverse of ('b -> 'a -> 'b)
      | Update of ('b -> old_value:'a -> new_value:'a -> 'b)
  end

  (** [unordered_array_fold ts ~init ~f ~update] folds over the [ts].  Unlike
      [array_fold], the fold will be computed in time proportional to the number of [ts]
      that change rather than the number of [ts].  In a stabilization, for each [t] in
      [ts] that changes from [old_value] to [new_value], the value of the unordered-array
      fold, [b], will change depending on [update]:

      - [F_inverse f_inverse]: from [b] to [f (f_inverse b old_value) new_value]
      - [Update update]: from [b] to [update b ~old_value ~new_value]

      The [t]'s that change may take effect in any order.

      If repeated changes might accumulate error, one can cause the fold to be fully
      computed after every [full_compute_every_n_changes] changes.  If you do not supply
      [full_compute_every_n_changes], then full computes will never happen after the
      initial one. *)
  val unordered_array_fold
    :  'w State.t
    -> ?full_compute_every_n_changes:int
    -> ('a, 'w) t array
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> update:('a, 'b) Unordered_array_fold_update.t
    -> ('b, 'w) t

  (** [opt_unordered_array_fold] is like [unordered_array_fold], except that its result is
      [Some] iff all its inputs are [Some]. *)
  val opt_unordered_array_fold
    :  'w State.t
    -> ?full_compute_every_n_changes:int
    -> ('a option, 'w) t array
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> f_inverse:('b -> 'a -> 'b)
    -> ('b option, 'w) t

  (** [sum ts ~zero ~add ~sub ?full_compute_every_n_changes] returns an incremental that
      maintains the sum of the [ts].  It uses [unordered_array_fold] so that the work
      required to maintain the sum is proportional to the number of [ts] that change
      (i.e. one [sub] and one [add] per change).

      [opt_sum] is like [sum], except that its result is [Some] iff all its inputs are
      [Some]. *)
  val sum
    :  'w State.t
    -> ?full_compute_every_n_changes:int
    -> ('a, 'w) t array
    -> zero:'a
    -> add:('a -> 'a -> 'a)
    -> sub:('a -> 'a -> 'a)
    -> ('a, 'w) t

  val opt_sum
    :  'w State.t
    -> ?full_compute_every_n_changes:int
    -> ('a option, 'w) t array
    -> zero:'a
    -> add:('a -> 'a -> 'a)
    -> sub:('a -> 'a -> 'a)
    -> ('a option, 'w) t

  (** [sum_int ts] = [sum ts ~zero:0 ~add:(+) ~sub:(-)] *)
  val sum_int : 'w State.t -> (int, 'w) t array -> (int, 'w) t

  (** [sum_float ts] is:

      {[
        sum ts ~zero:0.0 ~add:(+.) ~sub:(-.)
          ~full_compute_every_n_changes:(Array.length ts)
      ]}

      This uses [sum] for fast update, with a full recompute every [length ts] changes to
      cut down on floating-point error. *)
  val sum_float : 'w State.t -> (float, 'w) t array -> (float, 'w) t

  (** The stack of bind left-hand sides currently in effect is the current "scope".  In
      order to create a function in one scope and apply it in a different scope, one must
      manually save and restore the scope.  Essentially, the scope should be part of every
      closure that constructs incrementals.  For example:

      {[
        bind t1 ~f:(fun i1 ->
          let f t2 = map t2 ~f:(fun i2 -> i1 + i2) in
          bind t3 ~f:(fun i -> ... f ...);
          bind t4 ~f:(fun i -> ... f ...));
      ]}

      In the above code, the calls to [f] will create a map node that unnecessarily
      depends on the left-hand side of the most recent bind ([t3] or [t4]).  To eliminate
      the unnecessary dependence, one should save and restore the scope for [f]:

      {[
        bind t1 ~f:(fun i1 ->
          let scope = Scope.current state in
          let f t2 =
            Scope.within state scope ~f:(fun () -> map t2 ~f:(fun i2 -> i1 + i2)) in
          bind t3 ~f:(fun i -> ... f ...);
          bind t4 ~f:(fun i -> ... f ...))
      ]}

      [lazy_from_fun] and [memoize_fun] capture some common situations in which one would
      otherwise need to use [Scope.within]. *)
  module Scope : sig
    type 'w t

    (** [top] is the toplevel scope. *)
    val top : _ t

    (** [current ()] returns the scope in which a node would be created right now. *)
    val current : 'w State.t -> unit -> 'w t

    (** [within t f] runs [f] in scope [t], which causes all nodes created by [f] to be in
        scope [t].  An exception raised by [f] will be raised by [within] in the usual
        way. *)
    val within : 'w State.t -> 'w t -> f:(unit -> 'a) -> 'a
  end

  module Var : sig
    type ('a, 'w) t [@@deriving sexp_of]

    (** By default, a variable is created in [Scope.top], on the theory that its value
        depends on external stimuli (via [Var.set]), not on the current scope.  However,
        in some situations it is useful to supply [~use_current_scope:true] to create a
        variable that is invalidated when the current scope is invalidated, e.g. if one
        wants to use [on_update (watch var) ~f:(function Invalidated -> ... | ...)] to
        remove the external stimulus that was setting [var].

        It is allowed to do [let t = create a] during stabilization; for that
        stabilization, [watch t] will have value [a]. *)
    val create
      :  'w State.t
      -> ?use_current_scope:bool (** default is [false] *)
      -> 'a
      -> ('a, 'w) t

    (** [set t a] sets the value of [t] to [a].  Outside of stabilization, subsequent
        calls to [Var.value t] will see [a], but the [set] will not have any effect on
        incrementals until the next stabilization, at which point [watch t] will take on
        whatever [value t] was at the start of stabilization, causing incremental
        recomputation as usual.

        During a stabilization, calling [set] will behave as if [set] was called after
        stabilization finished: the new value will not be seen (by [value v] or [watch v])
        until after the stabilization finishes. *)
    val set : ('a, _) t -> 'a -> unit

    (** [watch t] returns an incremental that tracks the value of [t].  For a given [t],
        all calls to [watch t] return the same incremental. *)
    val watch : ('a, 'w) t -> ('a, 'w) incremental

    (** [value t] returns the value most recently [set] for [t] outside of
        stabilization. *)
    val value : ('a, _) t -> 'a

    (** [latest_value t] returns the value most recently [set] for [t].  It can differ
        from [value t] only during stabilization. *)
    val latest_value : ('a, _) t -> 'a

    (** [replace t ~f] = [set t (f (latest_value t))] *)
    val replace : ('a, _) t -> f:('a -> 'a) -> unit
  end

  module Observer : sig
    type ('a, 'w) t [@@deriving sexp_of]

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

    val observing : ('a, 'w) t -> ('a, 'w) incremental
    val use_is_allowed : _ t -> bool

    (** [value t] returns the current value of [t], or [Error] if [t] does not currently
        have a stable value.  In particular, [value t] will return [Error] in the
        following situations:

        - in the middle of stabilization.
        - if [stabilize] has not been called since [t] was created.
        - if [disallow_future_use t] has been called.
        - if [observing t] is invalid.

        Rather than using [value] in a function that runs during stabilization, one should
        use [map] or [bind] to express the dependence of an incremental computation on an
        incremental. *)
    val value : ('a, _) t -> 'a Or_error.t

    val value_exn : ('a, _) t -> 'a

    module Update : sig
      type 'a t =
        | Initialized of 'a
        | Changed of 'a * 'a (** [Changed (old_value, new_value)] *)
        | Invalidated
      [@@deriving compare, sexp_of]
    end

    (** [on_update_exn t ~f] calls [f] after the current stabilization and after each
        subsequent stabilization in which [t] changes, until [disallow_future_use t] is
        called.  [f] will be called at most once per stabilization.  Here is a state
        diagram for the allowable sequences of [Update.t]'s that can be supplied to a
        particular [f]:

        {v
           /-----------------------------------------------------\
           |                 /                                   |
           |                 |                                   v
          Start ------> Initialized ------> Changed ------> Invalidated
                                              ^  |
                                              \--/
        v}

        [on_update_exn] raises if [disallow_future_use t] was previously called. *)
    val on_update_exn : ('a, _) t -> f:('a Update.t -> unit) -> unit

    (** [disallow_future_use t] causes all future attempts to use [t] to fail and
        [on_update_exn] handlers added to [t] to never run again.  It also causes
        incremental to treat [t] as unobserved, and thus [stabilize] will not maintain the
        value of [t] or any of [t]'s descendants that are needed only to maintain [t].
        [disallow_future_use] raises if called during stabilization. *)
    val disallow_future_use : _ t -> unit
  end

  (** [observe t] returns a new observer for [t].  [observe] raises if called during
      stabilization.

      By default, an observer has a finalizer that calls [disallow_future_use] when the
      observer is no longer referenced.  One can use [~should_finalize:false] to cause the
      finalizer to not be created, in which case the observer will live until
      [disallow_future_use] is explicitly called. *)
  val observe
    :  ?should_finalize:bool (** default is [true] *)
    -> ('a, 'w) t
    -> ('a, 'w) Observer.t

  module Update : sig
    type 'a t =
      | Necessary of 'a
      | Changed of 'a * 'a (** [Changed (old_value, new_value)] *)
      | Invalidated
      | Unnecessary
    [@@deriving compare, sexp_of]
  end

  (** [on_update t ~f] is similar to [Observer.on_update_exn], but it does not cause [t]
      to be necessary.  Instead of the [Initialized] update, there are updates for when a
      node becomes [Necessary] or [Unnecessary].  Here is a state diagram for the
      allowable sequences of [Update.t]'s that can be supplied to a particular [f]:

      {v
        /-----------------------------------------------------\
        |                 /                                   |
        |                 |                                   v
       Start ------> Necessary ----------> Changed ------> Invalidated
        |                | ^             |  ^  |              ^
        |                | |   /---------/  \--/              |
        |                v |   v                              |
        \----------> Unnecessary -----------------------------/
      v}

      If [t] gets a new value during a stabilization but is unnecessary at the end of it,
      [f] will _not_ be called with [Changed], but with [Unnecessary] if allowed by the
      transition diagram.  I.e. if the prior call to [f] was with [Necessary] or
      [Changed], [f] will be called with [Unnecessary].  If the prior call to [f] was with
      [Invalidated] or [Unnecessary], then [f] will not be called.

      One should typically use [Observer.on_update_exn], unless the [Unnecessary] updates
      are needed. *)
  val on_update : ('a, _) t -> f:('a Update.t -> unit) -> unit

  (** {1 Stabilization} *)

  (** [stabilize ()] recomputes all incrementals that are necessary and stale.  I.e. it
      propagates changes from variables that have been set to the necessary incrementals
      that depend on them, stopping propagation as per cutoffs. *)
  val stabilize : _ State.t -> unit

  val am_stabilizing : _ State.t -> bool

  (** {1 Cutoffs} *)

  (** An ['a Cutoff.t] is a function that returns [true] if propagation of changes should
      be cutoff at a node based on the old value and the (possible) new value of the
      node. *)
  module Cutoff : sig
    type 'a t [@@deriving sexp_of]

    include Invariant.S1 with type 'a t := 'a t

    val create : (old_value:'a -> new_value:'a -> bool) -> 'a t
    val of_compare : ('a -> 'a -> int) -> 'a t
    val of_equal : ('a -> 'a -> bool) -> 'a t
    val always : _ t
    val never : _ t
    val phys_equal : _ t
    val poly_equal : _ t
    val should_cutoff : 'a t -> old_value:'a -> new_value:'a -> bool

    (** One can use [equal] in combination with [get_cutoff] to check if a node has a
        particular cutoff function.  [equal] uses [Core.phys_equal] for functional
        values supplied to [create] and [of_compare]. *)
    val equal : 'a t -> 'a t -> bool
  end

  (** [set_cutoff t cutoff] replaces the current cutoff function for [t] with [cutoff].
      [cutoff] will be called any time [t] is recomputed, with [old_value] being the value
      of [t] before the recomputation and [new_value] being the value that just
      recomputed.  If [cutoff ~old_value ~new_value], then [t]'s value will remain as
      [old_value] ([new_value] is discarded) and anything depending on [t] will not be
      recomputed (at least not because of [t]).  If [not (cutoff ~old_value ~new_value)],
      then [t]'s value will become [new_value], and all nodes depending on [t] will
      recomputed.

      A reasonable choice for [cutoff] is an equality function on ['a].

      The default cutoff for every node is [phys_equal].  For example, this means that a
      [unit incremental] would only fire once; to disable this, use [set_cutoff t
      Cutoff.never]. *)
  val set_cutoff : ('a, _) t -> 'a Cutoff.t -> unit

  val get_cutoff : ('a, _) t -> 'a Cutoff.t

  (** [lazy_from_fun f] is like [Lazy.from_fun f], except that the nodes created by [f]
      will be created in the scope in which [lazy_from_fun] was called, rather than in the
      scope of the piece of code that first forces the resulting lazy.  Not using this
      function when defining lazy values is likely to result in exceptions being thrown by
      incremental.  As a rule of thumb, all [lazy e] that might create incremental nodes
      should be replaced by [lazy_from_fun (fun () -> e)].

      As usual with [Lazy], if [f] raises, then that exception will be raised when calling
      [Lazy.force]. *)
  val lazy_from_fun : _ State.t -> (unit -> 'a) -> 'a Lazy.t

  val default_hash_table_initial_size : int


  (** [memoize_fun f hashable] returns a function [m] that is a memoized version of [f]
      that will run [f a] on each distinct [a] that [m] is applied to, memoize the result
      (in a hash table), and thereafter for [a], [m] will return the memoized result.

      When [m] is called, it uses [Scope.within] to run [f] in the scope that was in
      effect when [memoize_fun f] was called.  This is essential to correctly capture the
      dependence of nodes that [f] creates on values that [f] is closed over, which may in
      turn depend on the left-hand sides of binds in the scope in effect when [memoize_fun
      f] was called.  Furthermore, nodes that [f] creates do not depend on the scope in
      effect when [m] is called.

      [memoize_fun_by_key] is a generalization that allows one to memoize over values that
      contain a uniquely identifying key, but also have other data. *)
  val memoize_fun
    :  ?initial_size:int (** default is [4]. *)
    -> _ State.t
    -> 'a Base.Hashtbl.Key.t
    -> ('a -> 'b)
    -> ('a -> 'b) Staged.t

  val memoize_fun_by_key
    :  ?initial_size:int (** default is [4]. *)
    -> _ State.t
    -> 'key Base.Hashtbl.Key.t
    -> ('a -> 'key)
    -> ('a -> 'b)
    -> ('a -> 'b) Staged.t

  (** The weak versions of the memoization functions use a {!Weak_hashtbl} for the memo
      table.  This keeps a weak pointer to each result, and so the garbage collector
      automatically removes unused results.  Furthermore, [stabilize] removes the table
      entries whose result is unused.  *)
  val weak_memoize_fun
    :  ?initial_size:int (** default is [4]. *)
    -> _ State.t
    -> 'a Base.Hashtbl.Key.t
    -> ('a -> 'b Heap_block.t)
    -> ('a -> 'b Heap_block.t) Staged.t

  val weak_memoize_fun_by_key
    :  ?initial_size:int (** default is [4]. *)
    -> _ State.t
    -> 'key Base.Hashtbl.Key.t
    -> ('a -> 'key)
    -> ('a -> 'b Heap_block.t)
    -> ('a -> 'b Heap_block.t) Staged.t

  (** For debugging purposes, one can store an arbitrary [Info.t] in a node.  This will
      be displayed as part of a node in error messages. *)
  val user_info : _ t -> Info.t option

  val set_user_info : _ t -> Info.t option -> unit

  module Node_value : sig
    type 'a t =
      | Invalid
      | Necessary_maybe_stale of 'a option
      | Unnecessary_maybe_stale of 'a option
    [@@deriving sexp_of]
  end

  val node_value : ('a, _) t -> 'a Node_value.t

  module Packed : sig
    type t

    (** [save_dot file ts] outputs to [file] the DAG of nodes in [ts] and all their
        descendants, in dot format. *)
    val save_dot : string -> t list -> unit
  end

  val pack : _ t -> Packed.t

  (** [save_dot file] outputs to [file] the DAG of all necessary nodes, in dot format. *)
  val save_dot : _ State.t -> string -> unit

  (** This [Let_syntax] allows you to write expressions like

      {[
        let open Incr.Let_syntax in
        let%map_open some_incr = watch some_variable
        and another_incr = ...
        and ...
        in
        ...expression involving some_incr, another_incr, etc...
      ]}

      Note that this is less efficient than using [map3], [map4], etc., as the latter
      produces fewer intermediate nodes.  You can also use [let%mapn] syntax to use n-ary
      map functions efficiently. *)
  module Let_syntax : sig
    val ( >>| ) : ('a, 'w) t -> ('a -> 'b) -> ('b, 'w) t
    val ( >>= ) : ('a, 'w) t -> ('a -> ('b, 'w) t) -> ('b, 'w) t

    module Let_syntax : sig
      val bind : ('a, 'w) t -> f:('a -> ('b, 'w) t) -> ('b, 'w) t

      include Bind_n_gen with type ('a, 'w) t := ('a, 'w) t

      val map : ('a, 'w) t -> f:('a -> 'b) -> ('b, 'w) t

      include Map_n_gen with type ('a, 'w) t := ('a, 'w) t

      val both : ('a, 'w) t -> ('b, 'w) t -> ('a * 'b, 'w) t

      module Open_on_rhs : sig
        (** This is [Var.watch]. *)
        val watch : ('a, 'w) Var.t -> ('a, 'w) t
      end
    end
  end

  module Before_or_after : sig
    type t =
      | Before
      | After
    [@@deriving sexp_of]
  end

  module Step_function = Step_function

  (** Incremental has a timing-wheel-based clock, and lets one build incremental values
      that change as its time passes.  One must explicitly call [advance_clock] to change
      incremental's clock; there is no implicit call based on the passage of time. *)
  module Clock : sig
    type 'w t [@@deriving sexp_of]

    (** The default timing-wheel configuration, with one millisecond precision, and alarms
        allowed arbitrarily far in the future. *)
    val default_timing_wheel_config : Timing_wheel.Config.t

    val create
      :  'w State.t
      -> ?timing_wheel_config:Timing_wheel.Config.t
      -> start:Time_ns.t
      -> unit
      -> 'w t

    (** The [alarm_precision] of the underlying timing wheel. *)
    val alarm_precision : _ t -> Time_ns.Span.t

    val timing_wheel_length : _ t -> int

    (** [now t] returns the current time of incremental's clock. *)
    val now : _ t -> Time_ns.t

    (** [watch_now t] returns an incremental that tracks the current time. *)
    val watch_now : 'w t -> (Time_ns.t, 'w) incremental

    (** [advance_clock t ~to_] moves incremental's clock forward to [to_].
        [advance_clock] is a no-op if [to_ < now t].  As with [Var.set], the effect of
        [advance_clock] is not seen on incremental values until the next stabilization.
        Unlike [Var.set], calling [advance_clock] during stabilization raises.

        In certain pathological cases, [advance_clock] can raise due to it detecting a
        cycle in the incremental graph. *)
    val advance_clock : _ t -> to_:Time_ns.t -> unit

    (** [advance_clock_by t span = advance_clock t ~to_:(Time_ns.add (now t) span)] *)
    val advance_clock_by : _ t -> Time_ns.Span.t -> unit

    (** [at t time] returns an incremental that is [Before] when [now t < time] and
        [After] when [now t >= time]. *)
    val at : 'w t -> Time_ns.t -> (Before_or_after.t, 'w) incremental

    (** [after t span] is [at t (Time_ns.add (now t) span)]. *)
    val after : 'w t -> Time_ns.Span.t -> (Before_or_after.t, 'w) incremental

    (** [at_intervals t interval] returns an incremental whose value changes at time
        intervals of the form:

        {[
          Time_ns.next_multiple ~base ~after ~interval
        ]}

        where [base] is [now t] when [at_intervals] was called and [after] is the current
        [now t].

        [at_intervals] raises if [interval < alarm_precision].  The [unit t] that
        [at_intervals] returns has its cutoff set to [Cutoff.never], so that although its
        value is always [()], incrementals that depend on it will refire each time it is
        set.  The result of [at_intervals] remains alive and is updated until the
        left-hand side of its defining bind changes, at which point it becomes invalid. *)
    val at_intervals : 'w t -> Time_ns.Span.t -> (unit, 'w) incremental

    (** [step_function t ~init [(t1, v1); ...; (tn, vn)]] returns an incremental whose
        initial value is [init] and takes on the values [v1], ..., [vn] in sequence taking
        on the value [vi] when [now t >= ti].

        It is possible for [vi] to be skipped if time advances from [t(i-1)] to some time
        greater than [t(i+1)].

        The times must be in nondecreasing order, i.e. [step_function] raises if for some
        [i < j], [ti > tj]. *)
    val step_function : 'w t -> init:'a -> (Time_ns.t * 'a) list -> ('a, 'w) incremental

    (** [incremental_step_function t i] returns an incremental whose value is
        [Step_function.value f ~at:(now t)], where [f] is the value of [i]. *)
    val incremental_step_function
      :  'w t
      -> ('a Step_function.t, 'w) incremental
      -> ('a, 'w) incremental

    (** [snapshot t value_at ~at ~before] returns an incremental whose value is [before]
        before [at] and whose value is frozen to the value of [value_at] during the first
        stabilization in which the time passes [at].  [snapshot] causes [value_at] to be
        necessary during that stabilization even if the [snapshot] node itself is not
        necessary, but not thereafter (although of course [value_at] could remain
        necessary for other reaspons).  The result of [snapshot] will only be invalidated
        if [value_at] is invalid at the moment of the snapshot.

        [snapshot] returns [Error] if [at < now t], because it is impossible to take the
        snapshot because the time has already passed. *)
    val snapshot
      :  'w t
      -> ('a, 'w) incremental
      -> at:Time_ns.t
      -> before:'a
      -> ('a, 'w) incremental Or_error.t
  end

  (** A low-level, experimental interface to incremental.  This is useful when you need
      more control over the dependency graph, for performance reasons.  It comes at the
      cost that it's much harder to use right.  Specifically, here is what you can do with
      an expert node:

      - learn when any child changes, so the expert node can update itself incrementally,
        rather than having to look at the value of all its children.

      - incrementally update its set of parents.

      - select which parents should fire.

      If you use this interface, you are most definitely advised to test carefully, and in
      particular you should try it out using [incremental_debug], which is going to check
      most pre-conditions. *)
  module Expert : sig
    module Dependency : sig
      (** A [t] represents the edge from a child incremental to a parent expert node. A
          [t] is stateful, you cannot use the same [t] to link one child node to multiple
          parents at the same time. *)
      type ('a, 'w) t [@@deriving sexp_of]

      (** When calling [create ?on_change child], nothing happens until the [t] is linked
          to a parent.  see [Node.add_dependency] for documentation of [on_change]. *)
      val create : ?on_change:('a -> unit) -> ('a, 'w) incremental -> ('a, 'w) t

      (** [value t] reads the value of the child incremental.  It can only be used from
          the callback of the [Expert.Node.t] that has [t] in its set of dependencies. *)
      val value : ('a, _) t -> 'a
    end

    module Node : sig
      type ('a, 'w) t [@@deriving sexp_of]

      (** [let t = create ?on_observability_change callback] creates a new expert node.

          [on_observability_change], if given, is called whenever the node becomes
          observable or unobservable (with alternating value for [is_now_observable],
          starting at [true] the first time the node becomes observable).  This callback
          could run multiple times per stabilization.  It should not change the
          incremental graph.

          [callback] is called if any dependency of [t] has changed since it was last
          called, or if the set of dependencies has changed.  The callback will only run
          when all the dependencies are up-to-date, and inside the callback, you can
          safely call [Dependency.value] on your dependencies, as well as call all the
          functions below on the parent nodes.  Any behavior that works on all incremental
          nodes (cutoff, invalidation, debug info etc) also work on [t]. *)
      val create
        :  'w State.t
        -> ?on_observability_change:(is_now_observable:bool -> unit)
        -> (unit -> 'a)
        -> ('a, 'w) t

      (** [watch t] allows you to plug [t] in the rest of the incremental graph, but it's
          also useful to set a cutoff function, debug info etc. *)
      val watch : ('a, 'w) t -> ('a, 'w) incremental

      (** Calling [make_stale t] ensures that incremental will recompute [t] before
          anyone reads its value.  [t] may not fire though, if it never becomes
          necessary.  This is intended to be called only from a child of [t].  Along with
          a well chosen cutoff function, it allows to choose which parents should
          fire. *)
      val make_stale : _ t -> unit

      (** [invalidate t] makes [t] invalid, as if its surrounding bind had changed.  This
          is intended to be called only from a child of [t]. *)
      val invalidate : _ t -> unit

      (** [add_dependency t dep] makes [t] depend on the child incremental in the [dep].
          If [dep] is already used to link the child incremental to another parent, an
          exception is raised.

          This is intended to be called either outside of stabilization, or right after
          creating [t], or from a child of [t] (and in that case, as a consequence [t]
          must be necessary).

          The [on_change] callback of [dep] will be fired when [t] becomes observable,
          or immediately, or whenever the child changes as long as [t] is observable.
          When this function is called due to observability changes, the callback may
          fire several times in the same stabilization, so it should be idempotent. The
          callback must not change the incremental graph, particularly not the
          dependencies of [t].

          All the [on_change] callbacks are guaranteed to be run before the callback of
          [create] is run. *)
      val add_dependency : (_, 'w) t -> (_, 'w) Dependency.t -> unit

      (** [remove_dependency t dep] can only be called from a child of [t]. *)
      val remove_dependency : (_, 'w) t -> (_, 'w) Dependency.t -> unit
    end
  end

  module type S_gen = S_gen

  module type S = sig
    type state_witness [@@deriving sexp_of]

    include
      S_gen
      with type 'a t = ('a, state_witness) incremental
      with type Before_or_after.t = Before_or_after.t
      with type Clock.t = state_witness Clock.t
      with type 'a Cutoff.t = 'a Cutoff.t
      with type 'a Expert.Dependency.t = ('a, state_witness) Expert.Dependency.t
      with type 'a Expert.Node.t = ('a, state_witness) Expert.Node.t
      with type 'a Observer.t = ('a, state_witness) Observer.t
      with type 'a Observer.Update.t = 'a Observer.Update.t
      with type Packed.t = Packed.t
      with type Scope.t = state_witness Scope.t
      with type State.t = state_witness State.t
      with type State.Stats.t = State.Stats.t
      with type ('a, 'b) Unordered_array_fold_update.t =
             ('a, 'b) Unordered_array_fold_update.t
      with type 'a Update.t = 'a Update.t
      with type 'a Var.t = ('a, state_witness) Var.t
  end

  (** [Make] returns a new incremental implementation.  [Make] uses [Config.Default
      ()]. *)
  module Make () : S

  module Config : Config_intf.Config

  module type Incremental_config = Config.Incremental_config

  module Make_with_config (C : Incremental_config) () : S

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val debug : bool
  end
end

