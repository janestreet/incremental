#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"incremental"
  [ oasis_lib "incremental_lib"
  ; file "META" ~section:"lib"
  ]
