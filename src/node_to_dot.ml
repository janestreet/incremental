open! Core

let print_node (out : Format.formatter) ~name ~kind ~height ~user_info =
  let default = For_analyzer.Dot_user_info.default ~name ~kind ~height in
  let info =
    match user_info with
    | None -> default
    | Some user_info -> For_analyzer.Dot_user_info.append default user_info
  in
  Format.fprintf
    out
    "%s\n"
    (For_analyzer.Dot_user_info.to_string ~name (For_analyzer.Dot_user_info.to_dot info))
;;

let save_dot ~emit_bind_edges (out : Format.formatter) ts =
  let node_name =
    if am_running_test
    then fun _ -> "n###"
    else fun id -> "n" ^ For_analyzer.Node_id.to_string id
  in
  Format.fprintf out "digraph G {\n";
  Format.fprintf out "  rankdir = BT\n";
  let seen = For_analyzer.Node_id.Hash_set.create () in
  let bind_edges = ref [] in
  For_analyzer.traverse
    ts
    ~add_node:
      (fun
        ~id
        ~kind
        ~cutoff:_
        ~children
        ~bind_children
        ~user_info
        ~recomputed_at:_
        ~changed_at:_
        ~height
      ->
      let name = node_name id in
      Hash_set.add seen id;
      print_node out ~name ~kind ~height ~user_info;
      List.iter children ~f:(fun child_id ->
        Format.fprintf out "  %s -> %s\n" (node_name child_id) name);
      List.iter bind_children ~f:(fun bind_child_id ->
        bind_edges := (bind_child_id, id) :: !bind_edges));
  if emit_bind_edges
  then
    List.iter !bind_edges ~f:(fun (bind_child_id, id) ->
      if Hash_set.mem seen bind_child_id
      then
        Format.fprintf
          out
          "  %s -> %s [style=dashed]\n"
          (node_name id)
          (node_name bind_child_id));
  Format.fprintf out "}\n%!"
;;

let save_dot_to_file ~emit_bind_edges file ts =
  Out_channel.with_file file ~f:(fun out ->
    let formatter = Format.formatter_of_out_channel out in
    save_dot ~emit_bind_edges formatter ts)
;;
