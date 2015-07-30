open Core.Std

module Domain = Mimestring.Case_insensitive

    (* [prefix = None] means no brackets. *)
type t =
  { prefix : string option
  ; local_part : string
  ; domain : Domain.t option
  }
with fields, sexp, bin_io, compare

let create ?prefix ?domain local_part =
  { prefix
  ; local_part
  ; domain
  }
(* Comma-separated list:
     "A, B" <ab@x.com>, "C, D" <cd@x.com>
*)
let list_of_string_exn ?default_domain s =
  let module L = Email_address_lexer in
  L.parse_emails (Lexing.from_string s)
  |> List.map ~f:(fun { L. local_part; domain; prefix } ->
      let domain = Option.first_some domain default_domain in
      { local_part; domain; prefix })

let list_of_string ?default_domain s =
  Or_error.try_with (fun () -> list_of_string_exn ?default_domain s)

let of_string ?default_domain s =
  let open Or_error.Monad_infix in
  list_of_string ?default_domain s
  >>= function
  | [result] -> Ok result
  | _ -> Or_error.error_string ("Expected single email address: " ^ s)

let of_string_exn ?default_domain s =
  Or_error.ok_exn (of_string ?default_domain s)

let compose ~prefix ~address_part =
  match prefix with
  | None -> address_part
  | Some prefix -> sprintf "%s<%s>" prefix address_part

let to_string t =
  let address_part =
    match t.domain with
    | None -> t.local_part
    | Some domain -> sprintf "%s@%s" t.local_part domain
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

let set_prefix t prefix =
  { t with prefix }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn "local")
    ~expect: { local_part = "local"
             ; domain = None
             ; prefix = None }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn "<local>")
    ~expect: { local_part = "local"
             ; domain = None
             ; prefix = Some "" }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn " local@janestreet.com ")
    ~expect: { local_part = "local"
             ; domain = Some "janestreet.com"
             ; prefix = None }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn " <local@janestreet.com> ")
    ~expect: { local_part = "local"
             ; domain = Some "janestreet.com"
             ; prefix = Some "" }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn " John Doe <local> ")
    ~expect: { local_part = "local"
             ; domain = None
             ; prefix = Some "John Doe " }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn " John Doe <local@janestreet.com> ")
    ~expect: { local_part = "local"
             ; domain = Some "janestreet.com"
             ; prefix = Some "John Doe " }

TEST_UNIT =
  <:test_result<t>>
    (of_string_exn " \"Doe, John\" <local@janestreet.com> ")
    ~expect:{ local_part = "local"
            ; domain = Some "janestreet.com"
            ; prefix = Some "\"Doe, John\" " }

TEST_UNIT =
  <:test_result<t list>> (list_of_string_exn "") ~expect:[]

TEST_UNIT =
  <:test_result<t list>> (list_of_string_exn "   ") ~expect:[]

TEST_UNIT =
  <:test_result<t list>>
    (list_of_string_exn " \"Doe, John\" <local@janestreet.com>,
                       \n\t \"Doe, Johnny\" <local@janestreet.com> ")
    ~expect:[ { local_part = "local"
              ; domain = Some "janestreet.com"
              ; prefix = Some "\"Doe, John\" " }
            ; { local_part = "local"
              ; domain = Some "janestreet.com"
              ; prefix = Some "\"Doe, Johnny\" " }]

TEST_UNIT =
  <:test_result<t list>>
    (list_of_string_exn "x@y.com, \"a@b.com\" <\"mailto:a\"@b.com>")
    ~expect:[ { local_part = "x"
              ; domain = Some "y.com"
              ; prefix = None }
            ; { local_part = "\"mailto:a\""
              ; domain = Some "b.com"
              ; prefix = Some "\"a@b.com\" " } ]

let must_fail = function
  | Error _ -> ()
  | Ok ts ->
    failwithf "Expected to fail, got %s"
      (Sexp.to_string_hum (<:sexp_of<t list>> ts)) ()

TEST_UNIT =
  must_fail (list_of_string "mailnull@janestreet.com (Cron Daemon)")

TEST_UNIT =
  must_fail (list_of_string "a@b.com <a@b.com>")

TEST_UNIT =
  must_fail (list_of_string "a@@b.com")

let local_address () = create (Core.Std.Unix.getlogin ()) ~domain:(Core.Std.Unix.gethostname ())

module T = struct
  type nonrec t = t with sexp, bin_io, compare
  let hash = Hashtbl.hash
  let to_string = to_string
  let of_string s = of_string s |> Or_error.ok_exn
end
include Hashable.Make(T)
include Comparable.Make(T)
include Sexpable.Of_stringable(T)
