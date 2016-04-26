(* OASIS_START *)
(* OASIS_STOP *)
# 3 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch_default hook)
