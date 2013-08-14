(** Immutable sequences of bytes which can be windowed efficiently. *)
open Core.Std

type t =
  {
    text    : bool;
    content : Bigstring.t
  } with sexp;;


let fix_eol_in_place bstr =
  let len =
    let len = Bigstring.length bstr in
    Bigstring_extended.foldi bstr
      ~init:0
      ~f:(fun pos_src pos_dst c ->
        let next = pos_src + 1 in
        if c <> '\r' || next >= len || bstr.{next} <> '\n' then
        begin
          bstr.{pos_dst} <- c;
          pos_dst + 1
        end
        else
          pos_dst)
  in
  Bigstring.sub_shared ~len bstr
;;

let create ?(mode=`Text) ?(fix_win_eol=false) bstr =
  match mode with
  | `Text           ->
    {
      text = true;
      content =
        if fix_win_eol then
          fix_eol_in_place bstr
        else
          bstr
    }
  | `Binary         -> { text = false; content = bstr }
;;

let contents t = t.content

let mode_set t mode =
  { t with text =
    match mode with
    | `Text -> true
    | `Binary -> false
  }
;;

let mode t = if t.text then `Text else `Binary

let is_text t = t.text
let is_binary t = not (is_text t)

let empty = create Bigstring_extended.empty

let of_string str = create (Bigstring.of_string str)
let to_string t = Bigstring.to_string (contents t)

(** Bigstring.sub creates copies of the Bigstring *)
let of_bigstring bstr = create (Bigstring.subo bstr)
let to_bigstring t = Bigstring.subo (contents t)

let of_string_monoid mon = of_bigstring (String_monoid.to_bigstring mon)
let to_string_monoid t = String_monoid.of_bigstring (contents t)

let length t  = Bigstring.length t.content

let sub ?pos ?len t =
  let pos, len = match pos, len with
  | None, None         -> 0, length t
  | None, Some len     -> 0, len
  | Some pos, None     -> pos, ((length t) - pos)
  | Some pos, Some len -> pos, len
  in
  { t with content = Bigstring.sub_shared ~pos ~len t.content }
;;

let to_lexbuf t = Bigstring_extended.to_lexbuf t.content;;


TEST_MODULE "octet_stream" = struct
  open OUnit

  TEST "eol-fixing" = assert_equal ~printer:to_string
    (create ~mode:`Text ~fix_win_eol:true
      (Bigstring.of_string "aaa\r\naaa\naa\ra\r\r\na\r"))
    (create ~mode:`Text
      (Bigstring.of_string "aaa\naaa\naa\ra\r\na\r"));
    true
end

(********)

module Base64 = struct
  open OUnit

  let decode ?(mode=`Binary) t =
    (* Depending on encoding:
      If encoding is text, CRLF sequences are turned into LF.
      If encoding is binary, CRLF sequences are considered regular byte
        sequences.
    *)
    let bigbuffer, _ =
      Lexer.decode_base64
        ~is_text:(mode = `Text)
      (length t) (to_lexbuf t)
    in
    create ~mode (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

  let encode t =
    (* Depending on encoding:
      If t is text, all LF line endings are written as CRLF.
      If t is binary, do nothing.
    *)
    let bigbuffer =
      Lexer.encode_base64 ~is_text:(is_text t) (length t) (to_lexbuf t)
    in
    create ~mode:`Text
      (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

  TEST_MODULE "Octet_stream.Base64" = struct
    let pleasure = List.map ~f:(fun (x,y) -> (of_string x, of_string y))
      [
      ("YW55IGNhcm5hbCBwbGVhc3VyZS4=", "any carnal pleasure.");
      ("YW55IGNhcm5hbCBwbGVhc3VyZQ==", "any carnal pleasure" );
      ("YW55IGNhcm5hbCBwbGVhc3Vy"    , "any carnal pleasur"  );
      ("YW55IGNhcm5hbCBwbGVhc3U="    , "any carnal pleasu"   );
      ("YW55IGNhcm5hbCBwbGVhcw=="    , "any carnal pleas"    );
    ];;


    let test_decode pos l =
      let coded, plaintext = List.nth_exn l pos in
      let plaintext' = decode ~mode:`Text coded in
      assert_equal ~printer:to_string plaintext plaintext'
    ;;

    let test_encode pos l =
      let coded, plaintext = List.nth_exn l pos in
      let coded' = encode plaintext in
      assert_equal ~printer:to_string coded coded'
    ;;

    (** Exhaustive check of boundaries *)
    TEST_UNIT = test_decode 0 pleasure;;
    TEST_UNIT = test_decode 1 pleasure;;
    TEST_UNIT = test_decode 2 pleasure;;
    TEST_UNIT = test_decode 3 pleasure;;
    TEST_UNIT = test_decode 4 pleasure;;

    TEST_UNIT = test_encode 0 pleasure;;
    TEST_UNIT = test_encode 1 pleasure;;
    TEST_UNIT = test_encode 2 pleasure;;
    TEST_UNIT = test_encode 3 pleasure;;
    TEST_UNIT = test_encode 4 pleasure;;

  end

end

module Quoted_printable = struct
  open OUnit

  let decode ?mode t =
    (* The RFC2045 says that newlines can be converted to the platforms native
      format, so that's what we'll do.
      It's the same for both binary data and text data.
      If a CRLF sequence appears in the decoded data, that's because it
      was encoded as =0D=0A, which means the characters shouldn't be interpreted
      as EOL.
    *)
    let bigbuffer, _ = Lexer.decode_quoted_printable (length t) (to_lexbuf t) in
    create ?mode (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

  let encode t =
    let bigbuffer =
      Lexer.encode_quoted_printable (length t) ~is_text:(is_text t) (to_lexbuf t)
    in
    (* Even if the original string was binary content, it turns into text
      when quoted-printable-encoded. *)
    create ~mode:`Text (Bigstring_extended.of_bigbuffer_volatile bigbuffer)
  ;;

  TEST_MODULE "quoted-printable" = struct
    let mathematics = List.map ~f:(fun (x,y) -> (of_string x, of_string y))
      [("If you believe that truth=3Dbeauty, then surely =\n\
      mathematics is the most beautiful branch of philosophy.",
      "If you believe that truth=beauty, then surely mathematics is the \
      most beautiful branch of philosophy.")]
    ;;

    let encoding = List.map ~f:(fun (x,y) -> (of_string x, of_string y))
      [("=00=01=02a=03   =20\n", "\000\001\002a\003    \n");
       ("This text is fairly long and should be wrapped by a conforming =\n\
         implementation.",
        "This text is fairly long and should be wrapped by a conforming \
        implementation.");
       ("123456789A123456789B123456789C123456789D123456789E123456789F\
         123456789G12345=\n6789H123456789I",
        "123456789A123456789B123456789C123456789D123456789E123456789F\
        123456789G123456789H123456789I");
       ("123456789A123456789B123456789C123456789D123456789E123456789F\
         123456789G=3D23=\n456789H123456789I",
        "123456789A123456789B123456789C123456789D123456789E123456789F\
         123456789G=23456789H123456789I")
      ]
    ;;

    let test_decode pos l =
      let coded, plaintext = List.nth_exn l pos in
      let plaintext' = decode coded in
      assert_equal ~printer:to_string plaintext plaintext'
    ;;

    let test_encode pos l =
      let coded, plaintext = List.nth_exn l pos in
      let coded' = encode plaintext in
      assert_equal ~printer:to_string coded coded'
    ;;

    TEST_UNIT = test_decode 0 mathematics
    TEST_UNIT = test_encode 0 encoding
    TEST_UNIT = test_encode 1 encoding
    TEST_UNIT = test_encode 2 encoding
    TEST_UNIT = test_encode 3 encoding
  end

end

module Identity = struct
  let encode t = t
  let decode ?mode t =
    match mode with
    | Some mode -> mode_set t mode
    | None      -> t
end

