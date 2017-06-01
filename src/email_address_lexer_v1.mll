(* Parser for an RFC 5322 "address-list". Admittedly, it is not identical to spec. *)

(* This is used in stable code, think whether you need to mint a new version if
   you change this. *)
{
type email =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }

let fail_if_expect_comma expect_comma =
  if expect_comma then
    failwith "Email_address: unexpected character, expecting ','"
}

let whitespace = [' ' '\r' '\n' '\t']
let unquoted_prefix_char = [^ '<' '>' '@' ',' '"']
let quoted_prefix = '"' [^ '"']* '"'
let unquoted_prefix = (unquoted_prefix_char # whitespace) unquoted_prefix_char*
let prefix = quoted_prefix | unquoted_prefix
let address_char = [^ '<' '>' '@' ','] # whitespace
let domain_char = address_char # ['\'' '"']

let address_part =
  (address_char+ as local_part) ( '@' (domain_char+ as domain) )?

let email_without_prefix =
  address_part

let email_with_prefix =
  ((prefix whitespace*)? as prefix) '<' address_part '>'

rule parse_list expect_comma = parse
  | whitespace* ',' whitespace*
    { if expect_comma
      then parse_list false lexbuf
      else failwith "Email_address: unexpected ','"
    }
  | whitespace* eof
    { [] }
  | email_with_prefix
    { fail_if_expect_comma expect_comma;
      let email = { prefix = Some prefix; local_part; domain } in
      email :: parse_list true lexbuf
    }
  | email_without_prefix
    { fail_if_expect_comma expect_comma;
      let email = { prefix = None; local_part; domain } in
      email :: parse_list true lexbuf
    }

and parse_emails = parse
  | whitespace* { parse_list false lexbuf }
