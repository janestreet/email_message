open! Core.Core_stable
open! Core.Int.Replace_polymorphic_compare

module Stable_caseless_string = struct
  module U = Core.String.Caseless

  module V1 = struct
    module T = struct
      type t = String.V1.t [@@deriving sexp, bin_io, stable_witness, equal]

      let compare = U.compare
      let hash_fold_t = U.hash_fold_t

      type comparator_witness = U.comparator_witness

      let comparator = U.comparator
    end

    include T
    include Comparable.V1.With_stable_witness.Make (T)
  end
end

module Stable = struct
  module Domain = struct
    module V1 = Stable_caseless_string.V1
  end

  module V1 = struct
    module T = struct
      type t = Email_address_parser_stable_v1.t =
        { (* [prefix = None] means no brackets. *)
          prefix : String.V1.t option [@compare.ignore] [@hash.ignore]
        ; local_part : String.V1.t
        ; domain : Domain.V1.t option
        }
      [@@deriving fields, compare, hash, stable_witness]

      let equal = [%compare.equal: t]
      let create ?prefix ?domain local_part = { prefix; local_part; domain }

      let with_default_domain email ~default_domain =
        { email with domain = Core.Option.first_some email.domain default_domain }
      ;;

      let of_string ?default_domain input_str =
        let open Core in
        let open! Int.Replace_polymorphic_compare in
        match
          Angstrom.parse_string
            ~consume:Prefix
            Email_address_parser_stable_v1.email_only
            input_str
        with
        | Error error ->
          Or_error.error_s
            [%message
              "Failed to parse email address" (error : string) (input_str : string)]
        | Ok email -> Or_error.return (with_default_domain email ~default_domain)
      ;;

      let of_string_exn ?default_domain input_str =
        of_string ?default_domain input_str |> Core.Or_error.ok_exn
      ;;

      let compose ~prefix ~address_part =
        match prefix with
        | None -> address_part
        | Some prefix -> Core.sprintf "%s<%s>" prefix address_part
      ;;

      let to_string t =
        let address_part =
          match t.domain with
          | None -> t.local_part
          | Some domain -> Core.sprintf "%s@%s" t.local_part domain
        in
        compose ~prefix:t.prefix ~address_part
      ;;

      include Sexpable.Of_stringable.V1 (struct
          type nonrec t = t

          let to_string = to_string
          let of_string s = of_string_exn s
        end)

      let t_sexp_grammar = Sexplib0.Sexp_grammar.coerce [%sexp_grammar: string]

      include Binable.Of_stringable.V1 [@alert "-legacy"] (struct
          type nonrec t = t

          let to_string = to_string
          let of_string s = of_string_exn s
        end)
    end

    module With_comparator = struct
      include T
      include Comparator.V1.Make (T)
    end

    include With_comparator
    include Comparable.V1.With_stable_witness.Make (With_comparator)
  end
end

open Core
open! Int.Replace_polymorphic_compare

module Domain = struct
  include String.Caseless

  let to_string = Fn.id
  let of_string = Fn.id
end

module T = Stable.V1.With_comparator
include T

let list_of_string ?default_domain input_str =
  match
    Angstrom.parse_string
      ~consume:Prefix
      Email_address_parser_stable_v1.email_list_only
      input_str
  with
  | Error error ->
    Or_error.error_s
      [%message "Failed to parse email address(es)" (error : string) (input_str : string)]
  | Ok emails ->
    Or_error.return (List.map ~f:(with_default_domain ~default_domain) emails)
;;

let list_of_string_exn ?default_domain input_str =
  list_of_string ?default_domain input_str |> Core.Or_error.ok_exn
;;

let list_to_header_value ts = String.concat ~sep:",\n\t" (List.map ts ~f:to_string)

let address_part ?(brackets = false) ?(lowercase_domain = false) t =
  let prefix = if brackets then Some "" else None in
  let domain =
    if not lowercase_domain then t.domain else Option.map t.domain ~f:String.lowercase
  in
  { t with prefix; domain }
;;

let address_part_string ?brackets ?lowercase_domain t =
  to_string (address_part ?brackets ?lowercase_domain t)
;;

let set_address_part t address_part = of_string (compose ~prefix:t.prefix ~address_part)
let set_local_part t local_part = { t with local_part }
let set_domain t domain = { t with domain }
let set_prefix t prefix = { t with prefix }
let arg_type = Command.Arg_type.create of_string_exn

include Comparable.Make_plain_using_comparator (T)
include Hashable.Make_plain (T)

module Caseless = struct
  module T = struct
    type nonrec t = t =
      { prefix : String.Stable.V1.t option [@compare.ignore] [@hash.ignore]
      ; local_part : Stable_caseless_string.V1.t
      ; domain : Stable_caseless_string.V1.t option
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make_plain (T)
  include Comparable.Make_plain (T)
end
