open! Core
open! Core.Int.Replace_polymorphic_compare
open Angstrom
open Angstrom.Let_syntax

type t =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }
[@@deriving sexp_of]

let is_whitespace_char = function
  | ' ' | '\r' | '\n' | '\t' -> true
  | _ -> false
;;

let is_unquoted_prefix_char = function
  | '<' | '>' | '@' | ',' | '"' -> false
  | _ -> true
;;

let skip_whitespace = skip_while is_whitespace_char
let skip_while1 f = satisfy f *> skip_while f

let skip_satisfied_or_escaped1 f =
  skip_many1 (char '\\' *> advance 1 <|> skip_while1 (fun c -> Char.( <> ) '\\' c && f c))
;;

let quoted_prefix : unit Angstrom.t =
  let not_a_quote = Char.( <> ) '"' in
  char '"' *> option () (skip_satisfied_or_escaped1 not_a_quote) <* char '"'
;;

let unquoted_prefix : unit Angstrom.t =
  satisfy (fun c -> is_unquoted_prefix_char c && not (is_whitespace_char c))
  *> skip_while is_unquoted_prefix_char
;;

let prefix : string Angstrom.t =
  (* Includes the whitespace. *)
  consumed ((quoted_prefix <|> unquoted_prefix <?> "prefix") *> skip_whitespace)
;;

let is_address_char = function
  | '<' | '>' | '@' | ',' -> false
  | c -> not (is_whitespace_char c)
;;

let is_domain_char = function
  | '\'' | '"' -> false
  | c -> is_address_char c
;;

let local_part = consumed (skip_satisfied_or_escaped1 is_address_char) <?> "local_part"

let maybe_at_domain =
  option
    None
    (let%map (_ : char) = char '@'
     and domain = take_while1 is_domain_char in
     Some domain)
  <?> "domain"
;;

let email_without_prefix_no_quote =
  let%map local_part
  and domain = maybe_at_domain in
  local_part, domain
;;

let email_without_prefix_quote q = char q *> email_without_prefix_no_quote <* char q

let email_without_prefix_maybe_quote =
  (* Order matters here. Because [email_without_prefix_no_quote] may contain quotes in the
     local part we need to match the quoted version first. Otherwise we will
     'successfully' match the string and be left with an unconsumed trailing quote.
  *)
  email_without_prefix_quote '\''
  <|> email_without_prefix_quote '"'
  <|> email_without_prefix_no_quote
;;

let email_without_prefix =
  let%map local_part, domain = email_without_prefix_maybe_quote in
  { prefix = None; local_part; domain }
;;

let email_with_prefix =
  let%map prefix = option "" prefix
  and (_ : char) = char '<'
  and local_part, domain = email_without_prefix_maybe_quote
  and (_ : char) = char '>' in
  { prefix = Some prefix; local_part; domain }
;;

let email =
  (* Order matters here. `email_without_prefix` can (sometimes) parse the prefix of
     `email_with_prefix`. This means we must match `email_with_prefix` first, if this
     fails we fall back to `email_without_prefix`. (Otherwise [email_without_prefix] would
     match and then fail due to unconsumed input)
  *)
  email_with_prefix <|> email_without_prefix <?> "email"
;;

let email_list =
  let delim = skip_whitespace *> char ',' <* skip_whitespace in
  sep_by delim email <?> "email_list"
;;

let parse_only x = skip_whitespace *> x <* skip_whitespace <* end_of_input
let email_only = parse_only email
let email_list_only = parse_only email_list
