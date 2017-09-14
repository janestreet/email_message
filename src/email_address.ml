open Core.Core_stable


include struct
  open Core
  (* These don't have stable interfaces. *)
  module Int = Int
  module List = List
  module Option = Option
  module String = String
end

module Domain = Mimestring.Case_insensitive

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        { (* [prefix = None] means no brackets. *)
          prefix : String.t Option.t [@compare.ignore] [@hash.ignore]
        ; local_part : String.t
        ; domain : Domain.t Option.t
        } [@@deriving fields, sexp_of, compare, hash]
      (* The [sexp_of] converter defined here is only used for printing the output of
         unit tests. Externally we use a string representation - see bottom of this
         file. *)

      let create ?prefix ?domain local_part =
        { prefix
        ; local_part
        ; domain
        }

      (* Comma-separated list:
         "A, B" <ab@x.com>, "C, D" <cd@x.com>
      *)
      let list_of_string_exn ?default_domain s =
        let module L = Email_address_lexer_v1 in
        L.parse_emails (Lexing.from_string s)
        |> List.map ~f:(fun { L. local_part; domain; prefix } ->
          let domain = Option.first_some domain default_domain in
          { local_part; domain; prefix })

      let list_of_string ?default_domain s =
        let module Or_error = Core.Or_error in
        Or_error.try_with (fun () -> list_of_string_exn ?default_domain s)

      let of_string ?default_domain s =
        let module Or_error = Core.Or_error in
        let open Or_error.Monad_infix in
        list_of_string ?default_domain s
        >>= function
        | [result] -> Ok result
        | _ -> Or_error.error_string ("Expected single email address: " ^ s)

      let of_string_exn ?default_domain s =
        let module Or_error = Core.Or_error in
        Or_error.ok_exn (of_string ?default_domain s)

      let compose ~prefix ~address_part =
        match prefix with
        | None -> address_part
        | Some prefix -> Core.sprintf "%s<%s>" prefix address_part

      let to_string t =
        let address_part =
          match t.domain with
          | None -> t.local_part
          | Some domain -> Core.sprintf "%s@%s" t.local_part domain
        in
        compose ~prefix:t.prefix ~address_part

      let list_to_header_value ts =
        String.concat ~sep:",\n\t" (List.map ts ~f:to_string)

      let address_part ?(brackets = false) ?(lowercase_domain = false) t =
        let prefix = if brackets then Some "" else None in
        let domain =
          if not lowercase_domain
          then t.domain
          else Option.map t.domain ~f:String.lowercase
        in
        { t with prefix; domain }

      let address_part_string ?brackets ?lowercase_domain t =
        to_string (address_part ?brackets ?lowercase_domain t)

      let set_address_part t address_part =
        of_string (compose ~prefix:t.prefix ~address_part)

      let set_local_part t local_part = { t with local_part }
      let set_domain     t domain     = { t with domain }
      let set_prefix     t prefix     = { t with prefix }

      let local_address () =
        create (Core.Unix.getlogin ())

      include Sexpable.Of_stringable.V1(struct
          type nonrec t = t
          let to_string = to_string
          let of_string s = of_string s |> Core.Or_error.ok_exn
        end)

      include Binable.Of_stringable.V1(struct
          type nonrec t = t
          let to_string = to_string
          let of_string s = of_string s |> Core.Or_error.ok_exn
        end)
    end
    include T
    include Core.Comparable.Make_binable(T)
  end
end

open Core

include Stable.V1
include Hashable.Make_plain(Stable.V1)

module Caseless = struct
  module T = struct
    type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
  end
  include T
  include Hashable.Make_plain(T)
  include Comparable.Make_plain(T)
end
