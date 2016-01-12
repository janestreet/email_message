open Core.Std

module Params = struct
  type t = string Field_list.t [@@deriving sexp]

  let to_string_monoid t =
    let field_to_string_monoid ((name : Field_name.t), body) =
      let body = Rfc.RFC2045.Token.is_valid_or_quote body in
      String_monoid.concat_string [(name :> string); "="; body]
    in
    String_monoid.concat
      ~sep:(String_monoid.of_string "; ")
      (List.map t ~f:field_to_string_monoid)
  ;;
end

type t = {
  mime_type : Rfc.RFC2045.Token.t;
  mime_subtype : Rfc.RFC2045.Token.t;
  params : Params.t;
} [@@deriving fields, sexp]

let __UNUSED_VALUE__field_name = "content-type";;

let is ?a ?b t =
  Option.value_map a ~default:true
    ~f:(fun a ->
      Rfc.RFC2045.Token.equal t.mime_type (Rfc.RFC2045.Token.of_string a))
  &&
  Option.value_map b ~default:true
    ~f:(fun b ->
      Rfc.RFC2045.Token.equal t.mime_subtype (Rfc.RFC2045.Token.of_string b))
;;

(* Some convenience functions for working with mime types *)
let is_multipart t = is ~a:"multipart" t;;
let is_message_rfc2822 t = is ~a:"message" ~b:"rfc2822" t;;
let is_digest t = is ~a:"multipart" ~b:"digest" t;;

let is_composite t = is_multipart t || is_message_rfc2822 t
let is_simple t = not (is_composite t)
let is_text t = is ~a:"text" t

let mode t =
  if is_multipart t || is_text t || is_message_rfc2822 t then
    `Text
  else
    (* Unrecognized types are treated as application/octet-stream,
       that is, binary *)
    `Binary
;;

let multipart_boundary t =
  if is_multipart t
  then Option.map ~f:Boundary.create
         (Field_list.last t.params "boundary")
  else None
;;

let of_grammar (mime_type, mime_subtype, params) =
  { mime_type    = Rfc.RFC2045.Token.of_string mime_type
  ; mime_subtype = Rfc.RFC2045.Token.of_string mime_subtype
  ; params
  }
;;

let of_string x =
  of_grammar (Grammar.content_type
  Lexer.content_type (Lexing.from_string x))
;;

let to_string_monoid t =
  let mime_type = (t.mime_type :> string) in
  let mime_subtype = (t.mime_subtype :> string) in
  String_monoid.plus
    (String_monoid.concat_string [mime_type; "/"; mime_subtype; " "])
    (if List.is_empty t.params then
      String_monoid.empty
    else
      String_monoid.plus
      (String_monoid.of_string "; ")
      (Params.to_string_monoid t.params))
;;

let to_string t = String_monoid.to_string (to_string_monoid t);;
