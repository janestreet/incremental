open Core

module Store_params = struct
  type 'a t =
    | None : _ t
    | Map : { comparator : ('a, _) Comparator.Module.t } -> 'a t
    | Alist_lru :
        { equal : 'a -> 'a -> bool
        ; max_size : int
        }
        -> 'a t
    | Hashmap_lru :
        { lru : (module Lru_cache.S with type key = 'a)
        ; max_size : int
        }
        -> 'a t
    | With_hooks :
        { inner : 'a t
        ; if_found : 'a -> unit
        ; if_added : 'a -> unit
        }
        -> 'a t

  let map_based__store_forever comparator = Map { comparator }

  let alist_based__lru ~equal ~max_size =
    assert (max_size > 0);
    Alist_lru { equal; max_size }
  ;;

  let with_hooks inner ~if_found ~if_added = With_hooks { inner; if_found; if_added }

  let hash_based__lru
    (type key)
    ~max_size
    (module Key : Hashtbl.Key_plain with type t = key)
    : key t
    =
    let module Key : Lru_cache.H with type t = Key.t = struct
      include Key

      let invariant (_ : t) = ()
    end
    in
    let lru = (module Lru_cache.Make (Key) : Lru_cache.S with type key = Key.t) in
    Hashmap_lru { lru; max_size }
  ;;

  let none = None
end

module Store = struct
  type ('k, 'v) t =
    { find : 'k -> 'v option
    ; add : key:'k -> value:'v -> unit
    }

  let find_or_add t ~key ~default =
    match t.find key with
    | Some value -> `Found, value
    | None ->
      let value = default () in
      t.add ~key ~value;
      `Added, value
  ;;

  let find t key = t.find key
  let add t ~key ~value = t.add ~key ~value

  let rec create : type k. k Store_params.t -> (k, _) t =
    fun params ->
    match params with
    | None ->
      let find _ = None in
      let add ~key:_ ~value:_ = () in
      { find; add }
    | Map { comparator } ->
      let cache = ref (Map.empty comparator) in
      let find key = Map.find !cache key in
      let add ~key ~value = cache := Map.set !cache ~key ~data:value in
      { find; add }
    | Alist_lru { equal; max_size } ->
      let cache = ref [] in
      let find key =
        match List.Assoc.find !cache ~equal key with
        | Some value ->
          cache := (key, value) :: List.Assoc.remove !cache ~equal key;
          Some value
        | None -> None
      in
      let add ~key ~value = cache := (key, value) :: List.take !cache (max_size - 1) in
      { find; add }
    | Hashmap_lru { lru; max_size } ->
      let (module Lru : Lru_cache.S with type key = k) = lru in
      let cache = Lru.create ~max_size () in
      let find key = Lru.find cache key in
      let add ~key ~value = Lru.set cache ~key ~data:value in
      { find; add }
    | With_hooks { inner; if_found; if_added } ->
      let inner = create inner in
      let find key =
        let res = inner.find key in
        if Option.is_some res then if_found key;
        res
      in
      let add ~key ~value =
        inner.add ~key ~value;
        if_added key
      in
      { find; add }
  ;;
end

module Make (Incr : Incremental.S) = struct
  module Incr_with_store_params = struct
    type 'a t = 'a Incr.t * 'a Store_params.t
  end

  let with_params = Tuple2.create

  let bind (type a) ((x, store_params) : a Incr_with_store_params.t) ~(f : a -> 'b Incr.t)
    : 'b Incr.t
    =
    let scope = Incr.Scope.current () in
    let store = Store.create store_params in
    let%bind.Incr x in
    let default () = Incr.Scope.within scope ~f:(fun () -> f x) in
    let (`Found | `Added), graph = Store.find_or_add store ~key:x ~default in
    graph
  ;;

  let ( >>= ) = bind

  module Let_syntax = struct
    module Let_syntax = struct
      let bind = bind
    end
  end

  module Store_params = Store_params
  module Store = Store
end
