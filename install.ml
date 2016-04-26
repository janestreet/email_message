#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"email_message"
  [ oasis_lib "email_message"
  ; file "META" ~section:"lib"
  ; file "_build/namespace_wrappers/magic_mime_external.cmi" ~section:"lib"
  ]
