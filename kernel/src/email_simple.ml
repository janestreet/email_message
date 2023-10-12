open Core.Core_stable

module Stable = struct
  module Content = struct
    module V1 = Email.Stable.V1
  end

  module Mimetype = struct
    module V1 = struct
      type t = String.V1.t [@@deriving bin_io, compare, sexp]
    end
  end
end

open! Core
open Poly

let last_header ?normalize t name = Headers.last ?normalize (Email.headers t) name

let add_headers t headers' =
  Email.modify_headers t ~f:(fun headers -> Headers.add_all headers headers')
;;

let set_header_at_bottom t ~name ~value =
  Email.modify_headers t ~f:(Headers.set_at_bottom ~name ~value)
;;

module Expert = struct
  let content ~normalize_headers ~extra_headers ~encoding body =
    let headers =
      [ ( "Content-Transfer-Encoding"
        , Octet_stream.Encoding.to_string (encoding :> Octet_stream.Encoding.t) )
      ]
      @ extra_headers
    in
    let headers = Headers.of_list ~normalize:normalize_headers headers in
    let octet_stream =
      body |> Bigstring_shared.of_string |> Octet_stream.encode ~encoding
    in
    Email_content.to_email ~headers (Data octet_stream)
  ;;

  let multipart ~normalize_headers ~content_type ~extra_headers parts =
    (* [Multipart.create] will generate a suitable boundary, and [to_email] will ensure
       that this is added to the [Content-Type] header. *)
    let multipart = Email_content.Multipart.create parts in
    let headers = [ "Content-Type", content_type ] @ extra_headers in
    let headers = Headers.of_list ~normalize:normalize_headers headers in
    Email_content.to_email ~headers (Multipart multipart)
  ;;

  let create_raw
    ~from
    ~to_
    ?(cc = [])
    ?reply_to
    ~subject
    ~id
    ?in_reply_to
    ~date
    ?auto_generated
    ?(extra_headers = [])
    ?(attachments = [])
    content
    =
    let id, extra_headers =
      (* there seems to be some clients that set the [id] via extra_headers, so handle
         these correctly. *)
      let id_from_extra_headers =
        List.Assoc.find extra_headers "Message-Id" ~equal:String.Caseless.equal
      in
      let remove_id_from_extra_headers () =
        List.Assoc.remove extra_headers "Message-Id" ~equal:String.Caseless.equal
      in
      match id_from_extra_headers with
      | None -> id, extra_headers
      | Some id' ->
        if String.equal (String.strip id) (String.strip id')
        then id, remove_id_from_extra_headers ()
        else
          (* This case is odd, it will result in two different Message-Id headers on
             the email *)
          id, extra_headers
    in
    let headers =
      extra_headers
      @ [ "From", from ]
      @ (if List.is_empty to_ then [] else [ "To", String.concat to_ ~sep:",\n " ])
      @ (if List.is_empty cc then [] else [ "Cc", String.concat cc ~sep:",\n " ])
      @ (match reply_to with
         | None -> []
         | Some reply_to -> [ "Reply-To", reply_to ])
      @ [ "Subject", subject ]
      @ [ "Message-Id", id ]
      @ (match in_reply_to with
         | None -> []
         | Some in_reply_to -> [ "In-Reply-To", in_reply_to ])
      @ (match auto_generated with
         | None -> []
         | Some () -> [ "Auto-Submitted", "auto-generated"; "Precedence", "bulk" ])
      @ [ "Date", date ]
    in
    match attachments with
    | [] -> add_headers content headers
    | attachments ->
      multipart
        ~normalize_headers:`Whitespace
        ~content_type:"multipart/mixed"
        ~extra_headers:headers
        (set_header_at_bottom content ~name:"Content-Disposition" ~value:"inline"
         :: List.map attachments ~f:(fun (name, content) ->
              let content_type =
                last_header content "Content-Type"
                |> Option.value ~default:"application/x-octet-stream"
              in
              set_header_at_bottom
                content
                ~name:"Content-Type"
                ~value:(sprintf "%s; name=%s" content_type (Mimestring.quote name))
              |> set_header_at_bottom
                   ~name:"Content-Disposition"
                   ~value:(sprintf "attachment; filename=%s" (Mimestring.quote name))))
  ;;
end

module Mimetype = struct
  type t = Stable.Mimetype.V1.t [@@deriving compare, sexp_of]

  let text = "text/plain"
  let text_utf8 = "text/plain; charset=\"UTF-8\""
  let html = "text/html"
  let html_utf8 = "text/html; charset=\"UTF-8\""
  let pdf = "application/pdf"
  let jpg = "image/jpeg"
  let png = "image/png"
  let csv = "text/csv"
  let multipart_mixed = "multipart/mixed"
  let multipart_alternative = "multipart/alternative"
  let multipart_related = "multipart/related"
  let of_string t = t
  let equal = [%compare.equal: t]
  let arg_type = Command.Arg_type.create of_string
  let from_extension ext = Magic_mime_external.Mime_types.map_extension ext
  let from_filename file = Magic_mime_external.Magic_mime.lookup file

  let to_extension t =
    match
      String.lsplit2 t ~on:';'
      |> Option.value_map ~f:fst ~default:t
      |> String.lowercase
      |> String.strip
    with
    (* reverse mapping of the types listed above *)
    | "text/plain" -> Some "txt"
    | "text/html" -> Some "html"
    | "application/pdf" -> Some "pdf"
    | "image/jpeg" -> Some "jpg"
    | "image/png" -> Some "png"
    | "image/gif" -> Some "gif"
    | "text/csv" -> Some "csv" (* experimentally found from real-world examples *)
    | "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" -> Some "xlsx"
    | "application/vnd.openxmlformats-officedocument.wordprocessingml.document" ->
      Some "docx"
    | _ -> None
  ;;

  let guess_encoding : t -> Octet_stream.Encoding.known = function
    | "text/plain" | "text/html" -> `Quoted_printable
    | _ -> `Base64
  ;;
end

type attachment_name = string [@@deriving sexp_of]

module Content = struct
  type t = Email.t [@@deriving sexp_of]

  let of_email = Fn.id

  let create_custom
    ~content_type
    ?(encoding = Mimetype.guess_encoding content_type)
    ?(extra_headers = [])
    content
    =
    Expert.content
      ~normalize_headers:`Whitespace
      ~extra_headers:(extra_headers @ [ "Content-Type", content_type ])
      ~encoding
      content
  ;;

  let create = create_custom

  let html_utf8 ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.html_utf8 ~encoding content
  ;;

  let text_utf8 ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.text_utf8 ~encoding content
  ;;

  let create_multipart ?(extra_headers = []) ~content_type = function
    | [] -> failwith "at least one part is required"
    | [ content ] -> add_headers content extra_headers
    | parts ->
      Expert.multipart ~normalize_headers:`Whitespace ~content_type ~extra_headers parts
  ;;

  let alternatives ?extra_headers =
    create_multipart ?extra_headers ~content_type:Mimetype.multipart_alternative
  ;;

  let html_pre ?(force_no_line_wrap = true) str =
    (* This was copy&pasted from [markup.ml] in [html] library
       to avoid adding a dependency on the whole [html] library
       just for this. *)
    let html_encode s =
      let escape =
        [ "&", "&amp;"; "<", "&lt;"; ">", "&gt;"; "\"", "&quot;"; "'", "&#39;" ]
      in
      List.fold ~init:s escape ~f:(fun acc (pattern, with_) ->
        String.substr_replace_all acc ~pattern ~with_)
    in
    let pre content =
      let open_tag =
        if force_no_line_wrap
        then
          (* Gmail decided that text in "pre" elements should wrap by default
             (white-space=pre-wrap), the white-space rule here prevents wrapping and takes
             precendence over their rule. *)
          "<pre style=\"white-space: pre !important;\">"
        else "<pre>"
      in
      let close_tag = "</pre>" in
      [%string "%{open_tag}%{content}%{close_tag}"]
    in
    "<html>" ^ pre (html_encode str) ^ "</html>"
  ;;

  let text_monospace_utf8 ?extra_headers ?force_no_line_wrap content =
    alternatives
      ?extra_headers
      [ text_utf8 ?encoding:None content
      ; html_utf8 ?encoding:None (html_pre ?force_no_line_wrap content)
      ]
  ;;

  let mixed ?extra_headers =
    create_multipart ?extra_headers ~content_type:Mimetype.multipart_mixed
  ;;

  let with_related ?(extra_headers = []) ~resources t =
    Expert.multipart
      ~normalize_headers:`Whitespace
      ~content_type:Mimetype.multipart_related
      ~extra_headers
      (* per RFC2183 only the first part gets [Content-Disposition:inline],
         The related parts should not be displayed sequentially but are (presumably)
         referenced in the actual content. *)
      (add_headers t [ "Content-Disposition", "inline" ]
       :: List.map resources ~f:(fun (name, content) ->
            add_headers content [ "Content-Id", sprintf "<%s>" name ]))
  ;;

  let parse_last_header t name =
    match last_header t name with
    | None -> None
    | Some str ->
      (match String.split str ~on:';' |> List.map ~f:String.strip with
       | [] -> None
       | v :: args ->
         let args =
           List.map args ~f:(fun str ->
             match String.lsplit2 str ~on:'=' with
             | None -> str, None
             | Some (k, v) -> String.strip k, Some (String.strip v))
         in
         Some (v, args))
  ;;

  let content_type t =
    let open Option.Let_syntax in
    parse_last_header t "Content-Type"
    >>| fst
    |> Option.value ~default:"application/x-octet-stream"
  ;;

  let strip_character_set_and_language filename =
    (* A value with a character_set and language looks like:

       us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A

       i.e. the character_set, language, and value are separated by a single quote *)
    match
      String.substr_index filename ~pattern:"'" ~pos:1
      |> Option.bind ~f:(fun pos ->
           String.substr_index filename ~pattern:"'" ~pos:(pos + 1))
      |> Option.map ~f:(fun pos -> pos + 1)
    with
    | Some pos -> String.subo filename ~pos
    | None -> filename
  ;;

  let unquote name =
    let len = String.length name in
    if len >= 2 && name.[0] = '"' && name.[len - 1] = '"'
    then String.sub name ~pos:1 ~len:(len - 2)
    else name
  ;;

  (* See https://datatracker.ietf.org/doc/html/rfc2231

     This function deals with "parameter value continuations".
     Take the following header as an example:

     {v
     Content-Type: message/external-body; access-type=URL;
       URL*0="ftp://";
       URL*1="cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar"
     v}

     [attribute_name] in this case is "URL" *)
  let handle_value_continuation ~all_parameters ~attribute_name =
    let matching_attributes =
      (* Filter out attributes that aren't part of the [attribute_name] continuation. We
         know the filtered list with be non-empty because all the call-sites are inside an
         if branch that did a prefix check. *)
      List.filter
        all_parameters
        ~f:(fun (fragment_attribute, (_ : attachment_name option)) ->
        String.Caseless.is_prefix fragment_attribute ~prefix:(attribute_name ^ "*"))
      |> Nonempty_list.of_list_exn
    in
    match matching_attributes with
    | [ (_, maybe_value) ] ->
      Option.value maybe_value ~default:"" |> unquote |> strip_character_set_and_language
    | _ :: _ as matching_attributes ->
      (* RFC2231 3.(2) specifies the filename numbering mechanism must not depend on
         ordering, since MIME states that parameters are not order sensitive.

         To address this, we strip out digits from the attribute name and sort on these to
         order the fragments. *)
      let matching_attributes =
        Nonempty_list.to_list matching_attributes
        |> List.map ~f:(fun (fragment_attribute, value) ->
             (* The name of an attribute that is part of a continuation is either NAME*DIGIT
             or NAME*DIGIT*. *)
             match String.split fragment_attribute ~on:'*' with
             | _ :: digit :: _ when String.for_all digit ~f:Char.is_digit ->
               let digit = Int.of_string digit in
               Ok (digit, value)
             | _ -> Error "multipart-fragment-construction-failed")
        |> Result.all
      in
      (match matching_attributes with
       | Error error -> error
       | Ok all_parameters ->
         List.sort all_parameters ~compare:(fun a b ->
           Comparable.lift Int.compare ~f:fst a b)
         |> List.map ~f:(fun (seq, name_fragment) ->
              let name_fragment = unquote (Option.value name_fragment ~default:"") in
              if seq = 0
              then
                (* Only attempt to strip the encoding from the zeroth fragment. *)
                strip_character_set_and_language name_fragment
              else name_fragment)
         |> String.concat ~sep:"")
  ;;

  let attachment_name_from_args args ~attribute_name =
    List.find_map args ~f:(fun (k, v) ->
      if String.Caseless.equal k attribute_name
      then Option.map ~f:unquote v
      else if (* RFC2231 support *)
              String.Caseless.is_prefix k ~prefix:(attribute_name ^ "*")
      then Some (handle_value_continuation ~all_parameters:args ~attribute_name)
      else None)
  ;;

  let attachment_name t =
    let open Option.Let_syntax in
    Option.first_some
      (let%bind disp, args = parse_last_header t "Content-Disposition" in
       if String.Caseless.equal disp "attachment"
       then attachment_name_from_args args ~attribute_name:"filename"
       else None)
      (let%bind _, args = parse_last_header t "Content-Type" in
       attachment_name_from_args args ~attribute_name:"name")
  ;;

  let related_part_cid t =
    let open Option.Let_syntax in
    let%map str = last_header t "Content-Id" >>| String.strip in
    String.chop_prefix str ~prefix:"<"
    >>= String.chop_suffix ~suffix:">"
    |> Option.value ~default:str
  ;;

  let content_disposition t =
    match parse_last_header t "Content-Disposition" with
    | None -> `Inline
    | Some (disp, _) ->
      if String.Caseless.equal disp "inline"
      then `Inline
      else if String.Caseless.equal disp "attachment"
      then `Attachment (attachment_name t |> Option.value ~default:"unnamed-attachment")
      else (
        match attachment_name t with
        | None -> `Inline
        | Some name -> `Attachment name)
  ;;

  let parts t =
    match Email_content.parse t with
    | Error _ -> None
    | Ok (Email_content.Multipart ts) -> Some ts.Email_content.Multipart.parts
    | Ok (Message _) -> None
    | Ok (Data _) -> None
  ;;

  let content t =
    match Email_content.parse t with
    | Error _ -> None
    | Ok (Email_content.Multipart _) -> None
    | Ok (Message _) -> None
    | Ok (Data data) -> Some data
  ;;

  let rec inline_parts t =
    match parts t with
    | Some parts ->
      if String.Caseless.equal (content_type t) Mimetype.multipart_alternative
      then
        (* multipart/alternative is special since an aplication is expected to
           present/process any one of the alternative parts. The logic for picking
           the 'correct' alternative is application dependant so leaving this to
           to users (e.g. first one that parses) *)
        [ t ]
      else List.concat_map parts ~f:inline_parts
    | None ->
      (match content_disposition t with
       | `Inline -> [ t ]
       | `Attachment _ -> [])
  ;;

  let rec alternative_parts t =
    match parts t with
    | None -> [ t ]
    | Some ts ->
      if String.Caseless.equal (content_type t) Mimetype.multipart_alternative
      then List.concat_map ts ~f:alternative_parts
      else [ t ]
  ;;

  let rec all_related_parts t =
    let get_cid t =
      Option.map (related_part_cid t) ~f:(fun cid -> [ cid, t ])
      |> Option.value ~default:[]
    in
    get_cid t
    @ (parts t |> Option.value ~default:[] |> List.concat_map ~f:all_related_parts)
  ;;

  let find_related t name =
    List.find (all_related_parts t) ~f:(fun (cid, _t) -> String.equal cid name)
    |> Option.map ~f:snd
  ;;

  (* The following are considered deprecated since they leave the charset unspecified
     which has caused issues with some emails not displaying as expected. *)
  let text ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.text ~encoding content
  ;;

  let html ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.html ~encoding content
  ;;

  let text_monospace ?extra_headers ?force_no_line_wrap content =
    alternatives
      ?extra_headers
      [ text ?encoding:None content
      ; html ?encoding:None (html_pre ?force_no_line_wrap content)
      ]
  ;;
end

type t = Email.t [@@deriving sexp_of]

let create
  ~from
  ~to_
  ?cc
  ?reply_to
  ~subject
  ~id
  ?in_reply_to
  ~date_string
  ?auto_generated
  ?extra_headers
  ?attachments
  content
  =
  Expert.create_raw
    ~from:(Email_address.to_string from)
    ~to_:(List.map to_ ~f:Email_address.to_string)
    ?cc:(Option.map cc ~f:(List.map ~f:Email_address.to_string))
    ?reply_to:(Option.map reply_to ~f:Email_address.to_string)
    ~subject
    ~id
    ?in_reply_to
    ~date:date_string
    ?auto_generated
    ?extra_headers
    ?attachments
    content
;;

let decode_last_header ?normalize name ~f t =
  Option.bind (last_header ?normalize t name) ~f:(fun v ->
    Option.try_with (fun () -> f v))
;;

let from = decode_last_header "From" ~f:Email_address.of_string_exn
let to_ = decode_last_header "To" ~f:Email_address.list_of_string_exn
let cc = decode_last_header "Cc" ~f:Email_address.list_of_string_exn

let subject =
  decode_last_header ~normalize:`Whitespace_and_encoded_words "Subject" ~f:Fn.id
;;

let id = decode_last_header "Message-Id" ~f:Fn.id

let rec extract_body_ext'
  ~(accept : (Mimetype.t * (string * string option) list) option -> 'format option)
  email
  : ('format * string) Sequence.t
  =
  match accept (Content.parse_last_header email "Content-Type") with
  | Some format ->
    (match
       Octet_stream.of_bigstring_shared
         ~encoding:(Octet_stream.Encoding.of_headers_or_default (Email.headers email))
         (Email.raw_content email |> Email_raw_content.to_bigstring_shared)
       |> Octet_stream.decode
     with
     | Some body -> Sequence.singleton (format, Bigstring_shared.to_string body)
     | None -> Sequence.empty)
  | None ->
    (match Email_content.parse email with
     | Ok (Multipart parts) ->
       (* Recursively find the acceptable parts *)
       Sequence.of_list parts.parts |> Sequence.concat_map ~f:(extract_body_ext' ~accept)
     | Error _ | Ok (Message _) | Ok (Data _) ->
       (* We already checked the content type above so no need to dig further here. *)
       Sequence.empty)
;;

let extract_body_ext ~accept ?order t =
  let parts = extract_body_ext' ~accept t in
  match order with
  | None -> Sequence.hd parts
  | Some order -> Sequence.min_elt parts ~compare:(Comparable.lift order ~f:fst)
;;

let extract_body ?(content_type = Mimetype.text) email =
  extract_body_ext
    ~accept:(function
      | None -> None
      | Some (type_, _) -> Option.some_if (String.Caseless.equal type_ content_type) ())
    email
  |> Option.map ~f:snd
;;

let all_related_parts = Content.all_related_parts
let find_related = Content.find_related
let inline_parts = Content.inline_parts
