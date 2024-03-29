module Stable = struct
  open Core.Core_stable

  module Base = struct
    module V1 = struct
      type t =
        [ `exists_header of string * Re2.Stable.V1_no_options.t
        | `all_headers of string * Re2.Stable.V1_no_options.t
        ]
      [@@deriving bin_shape, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cbce30b485bdbeb9e93285287a91e7f5 |}]
      ;;
    end
  end

  module V1 = struct
    type t = Base.V1.t Blang.V1.t [@@deriving bin_shape, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 96b50dce93691f115ea076e70421edf9 |}]
    ;;
  end
end

open Core
open! Import
module Regex = Re2

module Base = struct
  type t =
    (* When adding to this type, don't forget to add to examples below. *)
    [ `exists_header of string * Regex.t
    | `all_headers of string * Regex.t
    ]
  [@@deriving sexp_of]

  let matches' t headers =
    match t with
    | `exists_header (header, regex) ->
      let headers = Headers.find_all headers header in
      List.exists headers ~f:(Regex.matches regex)
    | `all_headers (header, regex) ->
      let headers = Headers.find_all headers header in
      List.for_all headers ~f:(Regex.matches regex)
  ;;

  let matches t email = matches' t (Email.headers email)

  let examples =
    [ `exists_header ("cc", Regex.of_string ".*@janestreet.com")
    ; `all_headers ("cc", Regex.of_string ".*@janestreet.com")
    ]
  ;;
end

type t = Base.t Blang.t [@@deriving sexp_of]

let matches' t headers = Blang.eval t (fun base -> Base.matches' base headers)
let matches t email = matches' t (Email.headers email)
let example : t = Blang.and_ (List.map Base.examples ~f:Blang.base)
