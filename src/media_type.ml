open Core.Std

module Params = struct
  type t = (Headers.Name.t * string) list [@@deriving sexp]

  let to_string_monoid t =
    let field_to_string_monoid ((name : Headers.Name.t), body) =
      let body = Rfc.RFC2045.Token.is_valid_or_quote body in
      String_monoid.concat_string [(name :> string); "="; body]
    in
    String_monoid.concat
      ~sep:(String_monoid.of_string "; ")
      (List.map t ~f:field_to_string_monoid)
  ;;
  let last t name =
    let name = Headers.Name.of_string name in
    List.fold t ~init:None ~f:(fun r (k,v) ->
        if Headers.Name.equal name k then Some v else r)
end

type t = {
  mime_type : Rfc.RFC2045.Token.t;
  mime_subtype : Rfc.RFC2045.Token.t;
  params : Params.t;
} [@@deriving sexp]

let create ?(params=[]) mime_type mime_subtype =
  let mime_type = Rfc.RFC2045.Token.of_string mime_type in
  let mime_subtype = Rfc.RFC2045.Token.of_string mime_subtype in
  { mime_type; mime_subtype; params }

let mime_type t = (t.mime_type :> string)
let mime_subtype t = (t.mime_subtype :> string)
let params t = t.params
let param t name =
  List.Assoc.find ~equal:Headers.Name.equal t.params name

let is ?mime_type ?mime_subtype t =
  Option.value_map mime_type ~default:true
    ~f:(fun mime_type ->
        Rfc.RFC2045.Token.equal t.mime_type (Rfc.RFC2045.Token.of_string mime_type))
  &&
  Option.value_map mime_subtype ~default:true
    ~f:(fun mime_subtype ->
        Rfc.RFC2045.Token.equal t.mime_subtype (Rfc.RFC2045.Token.of_string mime_subtype))
;;

(* Some convenience functions for working with mime types *)
let is_multipart t = is ~mime_type:"multipart" t;;
let is_digest t = is ~mime_type:"multipart" ~mime_subtype:"digest" t;;

let is_composite t = is_multipart t
let is_simple t = not (is_composite t)
let is_text t = is ~mime_type:"text" t

let mode t =
  if is_multipart t || is_text t then
    `Text
  else
    (* Unrecognized types are treated as application/octet-stream,
       that is, binary *)
    `Binary
;;

let multipart_boundary t =
  if is_multipart t
  then Option.map ~f:Boundary.create
      (Params.last t.params "boundary")
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

let last headers =
  Option.bind (Headers.last ~whitespace:`Raw headers "Content-Type")
    (fun field -> Option.try_with (fun () -> of_string field))

let set_at_bottom headers t =
  Headers.set_at_bottom headers ~name:"Content-Type" ~value:(to_string t)

let default_default =
  { mime_type = Rfc.RFC2045.Token.of_string "text";
    mime_subtype = Rfc.RFC2045.Token.of_string "plain";
    params = [(Headers.Name.of_string "charset","us-ascii")]
  }
;;

let default_digest =
  { mime_type = Rfc.RFC2045.Token.of_string "message";
    mime_subtype = Rfc.RFC2045.Token.of_string "rfc2822";
    params = []
  }
;;

let default ~parent =
  if Option.value_map parent ~f:is_digest ~default:false
  then default_digest
  else default_default
;;

let%test_module "Media_type" =
  (module struct

    let headers =
      ["Content-Type", "multipart/mixed;\nboundary=\"BOUNDARY\""]
      |> Headers.of_list ~whitespace:`Normalize

    let%test_unit _ =
      [%test_result: t]
        (last headers |> Option.value_exn)
        ~expect:{ mime_type = Rfc.RFC2045.Token.of_string "multipart"
                ; mime_subtype = Rfc.RFC2045.Token.of_string "mixed"
                ; params = ["boundary", "BOUNDARY"] }
  end)
