module Stable = struct
  open Core.Core_stable
  include Email_message_kernel.Simple.Stable

  module Attachment = struct
    module Id = struct
      module V1 = struct
        type t =
          { filename : string
          ; path : int list
          }
        [@@deriving bin_io, compare, sexp]
      end
    end
  end
end

open! Core
open! Import
module Unix = Core_unix
module Crypto = Crypto.Cryptokit
module Hash = Crypto.Hash

(* break dependency cycle *)
module Email = Email_message_kernel

include (
  Email_message_kernel.Simple :
    Email_message_kernel.Private.Email_simple_intf.Email_simple
  with module Stable := Email_message_kernel.Simple.Stable)

let make_id () =
  if Ppx_inline_test_lib.am_running
  then "{AUTO-GENERATED-ID}"
  else
    (* Trust that UUID does a good enough job at avoiding colision risks.
       Only UUID to avoid leaking any interesting information. Use [X-JS-*] headers instead, see [tracing_headers] below. *)
    sprintf !"<%{Uuid}@ocaml.async_smtp>" (Uuid_unix.create ())
;;

let local_address () = Email_address.create ?domain:None (Core_unix.getlogin ())

let bigstring_shared_to_file data file =
  let open Async in
  Deferred.Or_error.try_with
    ~run:`Schedule
    ~rest:`Log
    (fun () ->
       Writer.with_file file ~f:(fun w ->
         String_monoid.output_unix (Bigstring_shared.to_string_monoid data) w;
         Writer.flushed w))
;;

module Expert = struct
  include Expert

  let tracing_headers () =
    let get_value ~actual ~testing =
      if Ppx_inline_test_lib.am_running then testing else actual
    in
    let sending_header str =
      let prefix = "X-Sending" in
      [%string "%{prefix}-%{str}"]
    in
    let headers =
      [ ( sending_header "Host"
        , get_value ~actual:(Unix.gethostname ()) ~testing:"{HOSTNAME}" )
      ; sending_header "User", get_value ~actual:(Unix.getlogin ()) ~testing:"{USERNAME}"
      ; ( sending_header "Program"
        , get_value ~actual:Sys_unix.executable_name ~testing:"{EXECUTABLE_NAME}" )
      ]
    in
    headers
  ;;

  let%expect_test "tracing_headers" =
    print_s [%sexp (tracing_headers () : (string * string) list)];
    [%expect
      {|
      ((X-JS-Sending-Host {HOSTNAME}) (X-JS-Sending-User {USERNAME})
       (X-JS-Sending-Program {EXECUTABLE_NAME}))|}]
  ;;

  let create_raw
        ?(from = local_address () |> Email_address.to_string)
        ~to_
        ?cc
        ?reply_to
        ~subject
        ?id
        ?in_reply_to
        ?date
        ?auto_generated
        ?(extra_headers = [])
        ?attachments
        ?no_tracing_headers
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
      match id with
      | None ->
        (match id_from_extra_headers with
         | None -> make_id (), extra_headers
         | Some id -> id, remove_id_from_extra_headers ())
      | Some id ->
        (match id_from_extra_headers with
         | None -> id, extra_headers
         | Some id' ->
           if String.equal (String.strip id) (String.strip id')
           then id, remove_id_from_extra_headers ()
           else
             (* This case is odd, it will result in two different Message-Id headers on
                the email *)
             id, extra_headers)
    in
    let date =
      match date with
      | None -> Email_date.rfc822_date (Time_float.now ())
      | Some date -> date
    in
    let extra_headers =
      extra_headers
      @
      match no_tracing_headers with
      | Some `Because_not_using_standard_email_infra -> []
      | None -> tracing_headers ()
    in
    create_raw
      ~from
      ~to_
      ?cc
      ?reply_to
      ~subject
      ~id
      ?in_reply_to
      ~date
      ?auto_generated
      ~extra_headers
      ?attachments
      content
  ;;
end

type attachment_name = string [@@deriving sexp_of]

module Path : sig
  type t

  val root : t
  val child : t -> int -> t
  val to_int_list : t -> int list
end = struct
  type t = int list

  let root = []
  let child t i = i :: t
  let to_int_list t = List.rev t
end

module Attachment = struct
  module Id = struct
    type t = Stable.Attachment.Id.V1.t =
      { filename : string
      ; path : int list
      }
    [@@deriving compare, fields, sexp_of]
  end

  type t =
    { headers : Headers.t
    ; id : Id.t
    ; embedded_email : Email.t option
    (* These are expensive operations. Ensure they are only computed once, and
       lazily. *)
    ; decoded_filename : string Lazy.t
    ; raw_data : Bigstring_shared.t Or_error.t Lazy.t
    ; md5 : string Or_error.t Lazy.t
    ; sha256 : string Or_error.t Lazy.t
    }
  [@@deriving fields, sexp_of]

  let filename t = Id.filename t.id
  let decoded_filename t = Lazy.force t.decoded_filename
  let raw_data t = Lazy.force t.raw_data
  let md5 t = Lazy.force t.md5
  let sha256 t = Lazy.force t.sha256

  let to_hex digest =
    let result = Bytes.create (String.length digest * 2) in
    let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = int_of_char digest.[i] in
      Bytes.set result (2 * i) hex.[c lsr 4];
      Bytes.set result ((2 * i) + 1) hex.[c land 0xF]
    done;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result
  ;;

  let of_content' ?embedded_email ~headers ~filename ~path content =
    let decoded_filename =
      lazy
        (Encoded_word.decode filename
         |> function
         | Ok s -> s
         | Error _ -> filename)
    in
    let raw_data =
      lazy
        (Or_error.try_with (fun () ->
           Octet_stream.decode (Lazy.force content) |> Option.value_exn))
    in
    let compute_hash ~hash =
      lazy
        (match Lazy.force raw_data with
         | Error _ as err -> err
         | Ok data ->
           Or_error.try_with (fun () ->
             Crypto.hash_string (hash ()) (Bigstring_shared.to_string data) |> to_hex))
    in
    let md5 = compute_hash ~hash:Hash.md5 in
    let sha256 = compute_hash ~hash:Hash.sha256 in
    let id = { Id.filename; path = Path.to_int_list path } in
    { headers; id; embedded_email; decoded_filename; raw_data; md5; sha256 }
  ;;

  let of_content ~headers ~filename ~path content =
    of_content' ~headers ~filename ~path (lazy content)
  ;;

  let of_embedded_email ~headers ~filename ~path embedded_email =
    let content =
      lazy
        (Email.to_bigstring_shared embedded_email
         |> Octet_stream.of_bigstring_shared
              ~encoding:(Octet_stream.Encoding.of_headers_or_default headers))
    in
    of_content' ~embedded_email ~headers ~filename ~path content
  ;;

  let to_file t file =
    match raw_data t with
    | Error _ as err -> Async.return err
    | Ok data -> bigstring_shared_to_file data file
  ;;
end

module Content = struct
  include Content

  let of_file ?content_type ?encoding ?extra_headers file =
    let open Async in
    let%map content = Reader.file_contents file in
    let content_type =
      match content_type with
      | None -> Mimetype.from_filename file
      | Some content_type -> content_type
    in
    create_custom ~content_type ?encoding ?extra_headers content
  ;;

  let to_file t file =
    let open Async in
    match content t with
    | None ->
      Deferred.Or_error.errorf
        "The payload of this email is ambigous, you\n\
        \                  you should decompose the email further"
    | Some content ->
      (match Octet_stream.decode content with
       | None -> Deferred.Or_error.errorf "The message payload used an unknown encoding"
       | Some content -> bigstring_shared_to_file content file)
  ;;
end

let create
      ?from
      ~to_
      ?cc
      ?reply_to
      ~subject
      ?id
      ?in_reply_to
      ?date
      ?auto_generated
      ?extra_headers
      ?attachments
      ?no_tracing_headers
      content
  =
  Expert.create_raw
    ?from:(Option.map from ~f:Email_address.to_string)
    ~to_:(List.map to_ ~f:Email_address.to_string)
    ?cc:(Option.map cc ~f:(List.map ~f:Email_address.to_string))
    ?reply_to:(Option.map reply_to ~f:Email_address.to_string)
    ~subject
    ?id
    ?in_reply_to
    ?date:(Option.map date ~f:Email_date.rfc822_date)
    ?auto_generated
    ?extra_headers
    ?attachments
    ?no_tracing_headers
    content
;;

let parse_attachment ~include_inline_parts ?container_headers ~path t =
  let headers = Email.headers t in
  let as_attachment ~even_if_multipart ~filename =
    match Email_content.parse ?container_headers t with
    | Error _ -> None
    | Ok (Email_content.Multipart _) ->
      if even_if_multipart
      then Some (Attachment.of_embedded_email ~headers ~filename ~path t)
      else None
    | Ok (Message email) ->
      Some (Attachment.of_embedded_email ~headers ~filename ~path email)
    | Ok (Data content) -> Some (Attachment.of_content ~headers ~filename ~path content)
  in
  match Content.content_disposition (Content.of_email t) with
  | `Inline ->
    (match include_inline_parts with
     | `None -> None
     | (`Named | `Named_or_has_content_id) as include_inline_parts ->
       let%bind.Option filename =
         match Content.attachment_name (Content.of_email t) with
         | Some filename -> Some filename
         | None ->
           (match include_inline_parts with
            | `Named -> None
            | `Named_or_has_content_id ->
              let%map.Option name = Content.related_part_cid (Content.of_email t) in
              (match
                 Mimetype.to_extension (Content.content_type (Content.of_email t))
               with
               | None -> name
               | Some ext -> name ^ "." ^ ext))
       in
       as_attachment ~even_if_multipart:true ~filename)
  | `Attachment filename -> as_attachment ~even_if_multipart:false ~filename
;;

let map_attachments ?(include_inline_parts = `None) t ~f =
  let handle_possible_attachment ?container_headers ~path t =
    parse_attachment ~include_inline_parts ?container_headers ~path t
    |> function
    | None -> `Unchanged
    | Some attachment ->
      (match f attachment with
       | `Keep -> `Unchanged
       | `Keep_and_don't_recurse -> `Stop
       | `Replace attachment' -> `Changed attachment')
  in
  let rec loop ?container_headers t ~path =
    match Email_content.parse ?container_headers t with
    | Error _ -> `Unchanged
    | Ok (Data _data) ->
      (match handle_possible_attachment ?container_headers ~path t with
       | `Stop -> `Unchanged
       | (`Unchanged | `Changed _) as t -> t)
    | Ok (Message message) ->
      (match handle_possible_attachment ?container_headers ~path t with
       | `Changed t' -> `Changed t'
       | `Stop -> `Unchanged
       | `Unchanged ->
         (match loop message ?container_headers:None ~path:(Path.child path 0) with
          | `Unchanged -> `Unchanged
          | `Changed message' -> `Changed (Email_content.set_content t (Message message'))))
    | Ok (Multipart (mp : Email_content.Multipart.t)) ->
      (match
         List.fold_mapi mp.parts ~init:`Unchanged ~f:(fun i change_status t ->
           match
             loop ~container_headers:mp.container_headers ~path:(Path.child path i) t
           with
           | `Unchanged -> change_status, t
           | `Changed t -> `Changed, t)
       with
       | `Unchanged, _ -> `Unchanged
       | `Changed, parts' ->
         let mp' = Email_content.Multipart.set mp ~parts:parts' () in
         `Changed (Email_content.set_content t (Multipart mp')))
  in
  match loop ?container_headers:None ~path:Path.root t with
  | `Unchanged -> t
  | `Changed t -> t
;;

let all_attachments
      ?(include_inline_parts = `None)
      ?(look_through_attached_mails = true)
      t
  =
  let all_attachments = Queue.create () in
  let (_ : t) =
    map_attachments ~include_inline_parts t ~f:(fun attachment ->
      Queue.enqueue all_attachments attachment;
      match look_through_attached_mails with
      | true -> `Keep
      | false -> `Keep_and_don't_recurse)
  in
  Queue.to_list all_attachments
;;

let find_attachment t name =
  List.find (all_attachments t) ~f:(fun attachment ->
    String.equal (Attachment.filename attachment) name)
;;
