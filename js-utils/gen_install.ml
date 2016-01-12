(* Generate <package>.install from setup.log *)

#use "install_tags.ml"

module String_map = Map.Make(String)
let string_map_of_list =
  List.fold_left
    (fun acc (k, v) ->
       assert (not (String_map.mem k acc));
       String_map.add k v acc)
    String_map.empty

let lines_of_file fn =
  let ic = open_in fn in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file ->
      close_in ic;
      List.rev acc
    | line ->
      loop (line :: acc)
  in
  loop []

let read_setup_log () =
  lines_of_file "setup.log"
  |> List.map (fun line -> Scanf.sscanf line "%S %S" (fun tag arg -> (tag, arg)))

let read_setup_data () =
  lines_of_file "setup.data"
  |> List.map (fun line -> Scanf.sscanf line "%[^=]=%S" (fun k v -> (k, v)))

let remove_cwd =
  let prefix = Sys.getcwd () ^ "/" in
  let len_prefix = String.length prefix in
  fun fn ->
    let len = String.length fn in
    if len >= len_prefix && String.sub fn 0 len_prefix = prefix then
      String.sub fn len_prefix (len - len_prefix)
    else
      fn

let gen_section oc name files =
  let pr fmt = Printf.fprintf oc (fmt ^^ "\n") in
  pr "%s: [" name;
  List.iter
    (fun (src, dst) ->
       let src = remove_cwd src in
       let dst =
         match dst with
         | None -> Filename.basename src
         | Some fn -> fn
       in
       if src = dst then
         pr "  %S" src
       else
         pr "  %S {%S}" src dst)
    files;
  pr "]"

let rec filter_log tags log acc =
  match log with
  | [] -> acc
  | (tag, fname) :: rest ->
    match String_map.find tag tags with
    | exception Not_found -> filter_log tags rest acc
    | dst -> filter_log tags rest ((fname, dst) :: acc)

let () =
  let log = read_setup_log () in
  let setup_data = read_setup_data () in
  let ext_dll =
    match List.assoc "ext_dll" setup_data with
    | ext -> ext
    | exception Not_found -> ".so"
  in
  let merge name files map =
    match String_map.find name map with
    | files' -> String_map.add name (files @ files') map
    | exception Not_found -> String_map.add name files map
  in
  let sections =
    List.fold_left
      (fun acc (name, tags, extra_files) ->
         let tags = string_map_of_list tags in
         let files = filter_log tags log [] @ extra_files in
         if name = "lib" then
           let stubs, others =
             List.partition
               (fun (fn, _) -> Filename.check_suffix fn ext_dll)
               files
           in
           merge "lib" others (merge "stublibs" stubs acc)
         else
           merge name files acc)
      String_map.empty sections
    |> String_map.bindings
    |> List.filter (fun (_, l) -> l <> [])
  in
  let oc = open_out (package_name ^ ".install") in
  List.iter (fun (name, files) -> gen_section oc name files) sections;
  close_out oc
