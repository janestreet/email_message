open! Core

module Stable_caseless_string = struct
  module U = String.Caseless

  module V1 = struct
    module T = struct
      type t = String.Stable.V1.t [@@deriving sexp, bin_io ~localize, stable_witness]

      (* Caseless, like [compare]. *)
      include (
        U :
          sig
            type t [@@deriving compare ~localize, equal ~localize, hash]

            include Comparator.S with type t := t
          end
          with type t := U.t
           and type comparator_witness = U.comparator_witness)
    end

    include T
    include Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
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
          prefix : String.Stable.V1.t option [@compare.ignore] [@hash.ignore]
        ; local_part : String.Stable.V1.t
        ; domain : Domain.V1.t option
        }
      [@@deriving fields ~getters, compare ~localize, hash, stable_witness]

      let%template[@mode m = (global, local)] equal = ([%compare.equal: t] [@mode local])
      let create ?prefix ?domain local_part = { prefix; local_part; domain }

      let with_default_domain email ~default_domain =
        { email with domain = Option.first_some email.domain default_domain }
      ;;

      let of_string ?default_domain input_str =
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
        of_string ?default_domain input_str |> Or_error.ok_exn
      ;;

      let compose ~prefix ~address_part =
        match prefix with
        | None -> address_part
        | Some prefix -> sprintf "%s<%s>" prefix address_part
      ;;

      let compose_utf8 ~prefix ~address_part =
        match prefix with
        | None -> address_part
        | Some prefix ->
          let encoded_prefix = Base64.encode_string prefix in
          let prefix_utf8 = sprintf "=?%s?B?%s?=" "UTF-8" encoded_prefix in
          sprintf "%s<%s>" prefix_utf8 address_part
      ;;

      let to_string t =
        let address_part =
          match t.domain with
          | None -> t.local_part
          | Some domain -> sprintf "%s@%s" t.local_part domain
        in
        compose ~prefix:t.prefix ~address_part
      ;;

      let to_string_utf8 t =
        let address_part =
          match t.domain with
          | None -> t.local_part
          | Some domain -> sprintf "%s@%s" t.local_part domain
        in
        compose_utf8 ~prefix:t.prefix ~address_part
      ;;

      include Sexpable.Stable.Of_stringable.V1 [@modality portable] (struct
          type nonrec t = t

          let to_string = to_string
          let of_string s = of_string_exn s
        end)

      let t_sexp_grammar = Sexplib0.Sexp_grammar.coerce [%sexp_grammar: string]

      include
        Binable.Stable.Of_stringable.V1 [@modality portable] [@alert "-legacy"] (struct
          type nonrec t = t

          let to_string = to_string
          let of_string s = of_string_exn s
        end)
    end

    module With_comparator = struct
      include T
      include Comparator.Stable.V1.Make [@modality portable] (T)
    end

    include With_comparator

    include
      Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (With_comparator)
  end
end

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
  list_of_string ?default_domain input_str |> Or_error.ok_exn
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
let arg_type = (Command.Arg_type.create [@mode portable]) [%eta1 of_string_exn]

include Comparable.Make_plain_using_comparator [@mode local] [@modality portable] (T)
include Hashable.Make_plain [@modality portable] (T)

module Expert = struct
  module Parser = Email_address_parser_stable_v1
end

module For_test = struct
  type t = Email_address_parser_stable_v1.t [@@deriving sexp_of]
end

module Caseless = struct
  module T = struct
    type nonrec t = t =
      { prefix : String.Stable.V1.t option
           [@compare.ignore] [@equal.ignore] [@hash.ignore]
      ; local_part : Stable_caseless_string.V1.t
      ; domain : Stable_caseless_string.V1.t option
      }
    [@@deriving compare ~localize, equal ~localize, hash, sexp]
  end

  include T
  include Hashable.Make_plain [@modality portable] (T)
  include Comparable.Make_plain [@mode local] [@modality portable] (T)
end

include (
struct
  let quickcheck_generator_string @ portable =
    (String.gen' [@mode portable]) Char.gen_alphanum
  ;;

  [%%rederive
    type nonrec t = t =
      { prefix : string option
      ; local_part : string
      ; domain : string option
      }
    [@@deriving quickcheck ~portable]]

  let quickcheck_generator @ portable =
    (Quickcheck.Generator.filter [@mode portable]) quickcheck_generator ~f:(fun t ->
      to_string t |> of_string |> Result.is_ok)
  ;;
end :
  Quickcheckable.S with type t := t)
