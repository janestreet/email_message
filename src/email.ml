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

let%test_module _ =
  (module struct
    open Async

    let parse s =
      let parsed = of_string s in
      printf !"%{sexp:t}\n" parsed;
      let roundtripped = to_string parsed in
      printf !"Successfully roundtripped: %{sexp:bool}" (String.equal s roundtripped)
    ;;

    let%expect_test "simple" =
      parse
        "From: foo@bar.com\n\
         To: foo@bar.com\n\
         \n\
         hello world";
      let%bind () = [%expect {|
        ((headers ((From " foo@bar.com") (To " foo@bar.com")))
         (raw_content ("hello world")))
        Successfully roundtripped: true |}]
      in
      return ()
    ;;

    let%expect_test "no newlines" =
      parse
        "";
      let%bind () = [%expect {|
        ((headers ()) (raw_content ()))
        Successfully roundtripped: true |}]
      in
      (* Header lines should be terminated with "\n". We add the missing "\n" when we
         [to_string]. *)
      parse
        "Header: hello world";
      let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content ()))
        Successfully roundtripped: false |}]
      in
      return ()
    ;;

    let%expect_test "1 newline" =
      (* This is malformed. I could imagine having [None] for [raw_content] as well. *)
      parse
        "\n\
        ";
      let%bind () = [%expect {|
        ((headers ()) (raw_content ("")))
        Successfully roundtripped: true |}]
      in
      (* This is malformed. I could imagine having [Some ""] for [raw_content] as well. *)
      parse
        "Header: hello world\n\
        ";
      let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content ()))
        Successfully roundtripped: true |}]
      in
      (* This case is weird, see below for an explanation *)
      parse
        "Header: hello world\n\
         Body";
      let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content (Body)))
        Successfully roundtripped: false |}]
      in
      return ()
    ;;

    let%expect_test "2 newlines" =
      parse
        "\n\
         \n\
        ";
      let%bind () = [%expect {|
        ((headers ()) (raw_content ("\n")))
        Successfully roundtripped: true |}]
      in
      parse
        "Header: hello world\n\
         \n\
         Body";
      let%bind () = [%expect {|
        ((headers ((Header " hello world"))) (raw_content (Body)))
        Successfully roundtripped: true |}]
      in
      return ()
    ;;

    let%expect_test "weird headers" =
      (* Google and Exim both change to "body mode" (and adds a blank line) on the first
         line that "doesn't look like a header" for some slightly different
         interpretation of that phrase:

         - Google allows ASCII 32-57,59-126 (printable characters minus colon) in a
         header name

         - Exim allows ASCII 33-57,59-126 (printable characters minus colon minus space)
         in a header name

         RFC 5322 does not specify how to handle an invalid header line.  The following
         scenarios are possible:

         1) An invalid header (according to the above rules) is written somewhere in the
         header block.  Any following headers are incorrectly treated as the start of
         the message body.

         2) An MUA forgets to put a blank line between the header block and the body.
         As long as the first body line does not look like a header (according to the
         above rules), the "right thing" happens. *)
      (* Make sure we can handle the obsolete syntax of headers with whitespace before the
         colon. This doesn't roundtrip because we remove the whitespace before the ":"*)
      parse
        "From: foo@bar.com\n\
         Obsolete-header : hello world\n";
      let%bind () = [%expect {|
        ((headers ((From " foo@bar.com") (Obsolete-header " hello world")))
         (raw_content ()))
        Successfully roundtripped: false |}]
      in
      (* Whitespace should not be a part of a header field.  Google considers this a
         valid header.  Exim treats this as the start of the body. *)
      parse
        "From: foo@bar.com\n\
         Malformed header: hello world\n";
      let%bind () = [%expect {|
        ((headers ((From " foo@bar.com")))
         (raw_content ("Malformed header: hello world\n")))
        Successfully roundtripped: false |}]
      in
      (* RFC 5322 says that field names must contain at least 1 character, however
         Google and Exim both don't have this requirement. In addition, we get some
         messages in the wild that have broken headers like this. *)
      parse
        "From: foo@bar.com\n\
         : hello world\n";
      let%bind () = [%expect {|
        ((headers ((From " foo@bar.com") ("" " hello world"))) (raw_content ()))
        Successfully roundtripped: true |}]
      in
      return ()
    ;;
  end)
