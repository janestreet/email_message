open Core

module T = struct
  type t =
    { headers     : Headers.t
    ; raw_content : Bigstring_shared.t option
    } [@@deriving compare, fields, hash, sexp]
  ;;

  open Or_error.Monad_infix

  let create ~headers ~raw_content = { headers; raw_content = Some raw_content }

  let raw_content t =
    Option.value ~default:Bigstring_shared.empty t.raw_content

  let set_headers     t headers     = { t with headers }
  let set_raw_content t raw_content = { t with raw_content = Some raw_content }

  let modify_headers     t ~f = set_headers     t (f t.headers)
  let modify_raw_content t ~f = set_raw_content t (f (raw_content t))

  (* The default type of a message depends on the type of its parent,
     so we need to pass it around. *)
  let of_bigstring_shared bstr =
    let lexbuf = Bigstring_shared.to_lexbuf bstr in
    begin
      try Ok (Email_grammar.message
                (Email_lexer.message (Email_lexer_state.create ())) lexbuf)
      with _ ->
        (* Looks like lexer just throws Failure, not Parsing.Parse_error *)
        let pos = lexbuf.Lexing.lex_curr_p in
        Or_error.error_string
          (sprintf "Error parsing email at line %d, column %d"
             pos.Lexing.pos_lnum
             (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
    end
    >>| fun (`Message (headers, content_offset)) ->
    let headers = Headers.of_list ~whitespace:`Raw headers in
    let raw_content =
      match content_offset with
      | `Truncated -> None
      | `Bad_headers pos ->
        Some (Bigstring_shared.sub ~pos bstr)
      | `Content_offset pos ->
        Some (Bigstring_shared.sub ~pos bstr)
    in
    { headers; raw_content }
  ;;

  let of_string str =
    of_bigstring_shared (Bigstring_shared.of_string str)
    |> Or_error.ok_exn
  ;;

  let of_bigstring bstr =
    of_bigstring_shared (Bigstring_shared.of_bigstring bstr)
    |> Or_error.ok_exn
  ;;

  let of_bigbuffer buffer =
    of_bigstring (Bigbuffer.big_contents buffer)
  ;;

  (* Message bodies are optional. I highly doubt anybody would handle [None] differently
     from [Some ""], so we don't expose this detail. It allows us to be smarter with
     [to_string] so we don't add a newline. *)
  let to_string_monoid t =
    let optional_body =
      match t.raw_content with
      | None -> []
      | Some raw_content ->
        [ String_monoid.concat
            [ String_monoid.nl
            ; String_monoid.of_bigstring (Bigstring_shared.to_bigstring raw_content)
            ]
        ]
    in
    String_monoid.concat (Headers.to_string_monoid t.headers :: optional_body)
  ;;

  let to_string t = String_monoid.to_string (to_string_monoid t)
  let to_bigstring t = String_monoid.to_bigstring (to_string_monoid t)
  let to_bigstring_shared t = Bigstring_shared.of_string_monoid (to_string_monoid t)
end

include T

include Comparable.Make(T)
include Binable.Of_binable (Bigstring)
    (struct
      type nonrec t = t
      let to_binable = to_bigstring
      let of_binable = of_bigstring
    end)
