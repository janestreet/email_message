module Debug_in_this_directory = Debug
open Core.Std
open Core_extended.Std
module Debug = Debug_in_this_directory

module rec Multipart : sig
  type t =
    { boundary : Boundary.t
    ; prologue : Bigstring_shared.t option
    ; epilogue : Bigstring_shared.t option
    ; parts    : Message.t list
    } with sexp, compare

  (* Returns none if this is not a multipart message. *)
  val of_bigstring_shared
    :  media_type:Media_type.t
    -> boundary:Boundary.t
    -> Bigstring_shared.t
    -> t Or_error.t

  val map_data
    :  t
    -> f:(Octet_stream.t -> Octet_stream.t)
    -> t

  val hash : t -> int

  include String_monoidable.S with type t := t
end = struct
  type t = {
    boundary : Boundary.t;
    prologue : Bigstring_shared.t sexp_option;
    epilogue : Bigstring_shared.t sexp_option;
    parts    : Message.t list;
  } with sexp, compare

  let of_bigstring_shared ~media_type ~boundary bstr =
    let open Or_error.Monad_infix in
    let prologue, parts, epilogue = Boundary.split boundary bstr in
    Debug.run_debug (fun () ->
      eprintf "Boundary: %s; Part count: %d\n"
        (Boundary.to_string boundary) (List.length parts));
    List.map parts ~f:(fun part ->
      Or_error.tag
        (Message.of_bigstring_shared ~parent:(Some media_type) part)
        (sprintf "failed part:\n%s" (Bigstring_shared.to_string part)))
    |> Or_error.all
    >>= fun parts ->
    Ok
      { boundary
      ; prologue
      ; epilogue
      ; parts
      }
  ;;

  let map_data t ~f =
    { t with parts = List.map t.parts ~f:(Message.map_data ~f) }
  ;;

  let to_string_monoid t =
    if List.is_empty t.parts then
      match t.prologue, t.epilogue with
      | Some prologue, Some epilogue ->
        String_monoid.plus
          (Bigstring_shared.to_string_monoid prologue)
          (Bigstring_shared.to_string_monoid epilogue)
      | Some content, None | None, Some content ->
        Bigstring_shared.to_string_monoid content
      | None, None ->
        String_monoid.of_string "\n"
    else
      let parts = List.map t.parts ~f:Message.to_string_monoid in
      let boundary = Boundary.generate ~suggest:t.boundary () in
      (* Different types of boundaries that may appear in a message *)
      let boundary_open_first = boundary |> Boundary.Open_first.to_string_monoid in
      let boundary_open       = boundary |> Boundary.Open.to_string_monoid in
      let boundary_close      = boundary |> Boundary.Close.to_string_monoid in

    let prologue =
        t.prologue
        |> Option.value_map
          ~f:Bigstring_shared.to_string_monoid
          ~default:String_monoid.empty
    in
    let first_boundary =
      if Option.is_some t.prologue then boundary_open else boundary_open_first
    in
    let inner_boundary = boundary_open in
    let last_boundary = boundary_close in
    let epilogue =
        t.epilogue
        |> Option.value_map
          ~f:Bigstring_shared.to_string_monoid
          ~default:String_monoid.empty
    in
    String_monoid.concat
      [ prologue
      ; first_boundary
      ; String_monoid.concat ~sep:inner_boundary parts
      ; last_boundary
      ; epilogue
      ]

  let hash { boundary; prologue; epilogue; parts } =
    let x =
      Boundary.hash boundary,
      Option.value_map prologue
        ~f:Bigstring_shared.hash ~default:(Hashtbl.hash None),
      Option.value_map epilogue
        ~f:Bigstring_shared.hash ~default:(Hashtbl.hash None),
      Common.list_hash parts ~hash:Message.hash
    in
    Hashtbl.hash x
end
and Content : sig
  type t =
      Multipart of Multipart.t
    | Data of Octet_stream.t
    | Message of Message.t
  with sexp, compare
  ;;

  val empty : unit -> t

  val of_bigstring_shared :
    headers:Headers.t
    -> parent:(Media_type.t option)
    -> Bigstring_shared.t
    -> t Or_error.t

  val map_data
    :  t
    -> f:(Octet_stream.t -> Octet_stream.t)
    -> t

  val simple
    :  t
    -> [ `Message of Message.t
       | `Data of Octet_stream.t
       | `Multipart of Message.t list ]

  val hash : t -> int

  include String_monoidable.S with type t := t
end
= struct
  (* Message and multipart hold no encoding, as they must be encoded using
     7bit encoding with US-ASCII character set *)
  type t = Multipart of Multipart.t
         | Data of Octet_stream.t
         | Message of Message.t
  with sexp, compare
  ;;

  let empty () = Data Octet_stream.empty

  let of_bigstring_shared ~headers ~parent bstr =
    let open Or_error.Monad_infix in
    let media_type =
      Option.value (Headers.Content_type.last headers)
        ~default:(Headers.Content_type.default ~parent)
    in
    let encoding = Octet_stream.Encoding.of_headers_or_default headers in
    let octet_stream = Octet_stream.create ~encoding bstr in
    let decode octet_stream =
      match Octet_stream.decode octet_stream with
      | None ->
        Or_error.error "Unknown message encoding"
          encoding Octet_stream.Encoding.sexp_of_t
      | Some decoded_bstr -> Ok decoded_bstr
    in
    if Media_type.is_message_rfc2822 media_type
    then begin
      decode octet_stream
      >>= fun decoded_bstr ->
      Message.of_bigstring_shared ~parent:(Some media_type) decoded_bstr
      >>= fun msg ->
      Ok (Message msg)
    end
    else begin
      match Media_type.multipart_boundary media_type with
      | Some boundary ->
        (* According to Wikipedia, the content-transfer-encoding of a multipart
           type must always be "7bit", "8bit" or "binary" to avoid the
           complications that would be posed by multiple levels of decoding. In
           this case this decode call is free. *)
        decode octet_stream
        >>= fun decoded_bstr ->
        Multipart.of_bigstring_shared ~media_type ~boundary decoded_bstr
        >>= fun multipart ->
        Ok (Multipart multipart)
      | None ->
        Ok (Data octet_stream)
    end
  ;;

  let map_data t ~f =
    match t with
    | Multipart t ->
      Multipart (Multipart.map_data t ~f)
    | Data data ->
      Data (f data)
    | Message t ->
      Message (Message.map_data t ~f)
  ;;

  let to_string_monoid = function
    | Multipart multipart -> Multipart.to_string_monoid multipart
    | Message message -> Message.to_string_monoid message
    | Data octet_stream ->
      Octet_stream.to_string_monoid octet_stream
  ;;

  let simple = function
    | Message message -> `Message message
    | Multipart multipart -> `Multipart multipart.Multipart.parts
    | Data octet_stream -> `Data octet_stream
  ;;

  let hash = function
    | Multipart m ->
      Hashtbl.hash (0, Multipart.hash m)
    | Data d ->
      Hashtbl.hash (1, Octet_stream.hash d)
    | Message m ->
      Hashtbl.hash (2, Message.hash m)
end
and Message : sig
  type t with sexp, compare

  val empty : unit -> t

  val of_raw_content
    :  headers: Headers.t
    -> raw_content : Bigstring_shared.t
    -> t Or_error.t

  val of_bigstring_shared
    :  parent:(Media_type.t option)
    -> Bigstring_shared.t
    -> Message.t Or_error.t

  val to_bigstring_shared : t -> Bigstring_shared.t

  include String_monoidable.S with type t := t
  include Stringable.S with type t := t
  include Bigstringable.S with type t := t

  val headers : t -> Headers.t
  val content : t ->
    [ `Message of t | `Data of Octet_stream.t | `Multipart of (t list) ]
  ;;

  val set_headers : t -> Headers.t -> t

  val map_data
    : t -> f:(Octet_stream.t -> Octet_stream.t) -> t

  val raw_content : t -> Bigstring_shared.t

  val hash : t -> int
end = struct
  type t =
    { headers    : Headers.t
    ; line_break : bool
    ; content    : Content.t
    } with sexp, compare
  ;;

  open Or_error.Monad_infix

  let of_raw_content ~headers ~raw_content =
    Content.of_bigstring_shared ~headers ~parent:None raw_content
    >>= fun content ->
    Ok { headers; line_break=true; content }
  ;;

  let empty () =
    { headers = Headers.empty
    ; line_break = true
    ; content = Content.empty ()
    }

  let of_grammar ~parent bstr (`Message (headers, content_offset)) =
    let line_break, bstr =
    match content_offset with
    | `Truncated ->
      Debug.run_debug (fun () -> eprintf "Warning: Message truncated\n%!");
      false, Bigstring_shared.of_string ""
    | `Bad_headers pos ->
      false, Bigstring_shared.sub ~pos bstr
    | `Content_offset pos ->
      true, Bigstring_shared.sub ~pos bstr
    in
    Content.of_bigstring_shared ~headers ~parent bstr
    >>= fun content ->
    Ok { headers; line_break; content }
  ;;

  (* The default type of a message depends on the type of its parent,
     so we need to pass it around. *)
  let of_bigstring_shared ~parent bstr =
    let lexbuf = Bigstring_shared.to_lexbuf bstr in
    begin
      try Ok (Grammar.message
                (Lexer.message (Lexer_state.create ())) lexbuf)
      with _ ->
        (* Looks like lexer just throws Failure, not Parsing.Parse_error *)
        let pos = lexbuf.Lexing.lex_curr_p in
        Or_error.error_string
          (sprintf "Error parsing email at line %d, column %d"
             pos.Lexing.pos_lnum
             (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
    end
    >>= fun parse_result ->
    of_grammar ~parent bstr parse_result
  ;;

  let of_string str =
    of_bigstring_shared ~parent:None (Bigstring_shared.of_string str)
    |> Or_error.ok_exn
  ;;

  let of_bigstring bstr =
    of_bigstring_shared ~parent:None (Bigstring_shared.of_bigstring bstr)

  let map_data t ~f =
    { t with content = Content.map_data ~f t.content }
  ;;

  let raw_content t =
    Bigstring_shared.of_string_monoid (Content.to_string_monoid t.content)

  let to_string_monoid t =
    let sep = if t.line_break then String_monoid.nl else String_monoid.empty in
    String_monoid.concat ~sep [
      Headers.to_string_monoid t.headers;
      Content.to_string_monoid t.content
    ]
  ;;

  let to_string t = String_monoid.to_string (to_string_monoid t);;
  let to_bigstring t = String_monoid.to_bigstring (to_string_monoid t);;
  let to_bigstring_shared t =
    Bigstring_shared.of_string_monoid (to_string_monoid t);;

  let headers t = t.headers;;
  let content t = Content.simple t.content;;
  let set_headers t headers = { t with headers };;

  let hash { headers; line_break=_; content } =
    let x =
      Headers.hash headers,
      Content.hash content
    in
    Hashtbl.hash x
end

include Message
include Comparable.Make(Message)

include Binable.Of_binable (Bigstring)
          (struct
            type nonrec t = t
            let to_binable = to_bigstring
            let of_binable bs = of_bigstring bs |> Or_error.ok_exn
          end)

let of_bigbuffer buffer =
  of_bigstring (Bigbuffer.big_contents buffer)

module Simple = struct
  let create ~headers ~body =
    let headers = List.map headers ~f:(fun (name, value) ->
      (Field_name.of_string name, value))
    in
    let raw_content = Bigstring_shared.of_string body in
    of_raw_content ~headers ~raw_content

  let create_exn ~headers ~body =
    create ~headers ~body |> Or_error.ok_exn
  ;;
end

TEST_MODULE = struct
  let check s =
    let b = Bigbuffer.create 1000 in
    Bigbuffer.add_string b s;
    let result =
      Bigbuffer.big_contents b
      |> of_bigstring
      |> Or_error.ok_exn
      |> to_string
    in
    <:test_result<string>> ~expect:s result

  (* A message without headers must start with an empty line. *)
  TEST_UNIT = check "\n"
  TEST_UNIT = check "\nhello world"
  TEST_UNIT = check "\nhello world\n hello again\n"
end
