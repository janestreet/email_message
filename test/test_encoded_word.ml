open Core
open Email_message.Email_headers.Encoded_word

let%expect_test "decode" =
  let test str = printf "%S\n" (decode str |> Or_error.ok_exn) in
  test "hello = there?!?";
  [%expect {| "hello = there?!?" |}];
  test "hello\n there\n\tagain\n   my\nfriend";
  [%expect {| "hello there\tagain   my\nfriend" |}];
  (* Some test vectors from: https://tools.ietf.org/html/rfc2047 *)
  test "=?ISO-8859-1?Q?a?=";
  [%expect {| "a" |}];
  test "=?ISO-8859-1?Q?a?= b";
  [%expect {| "a b" |}];
  test "=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=";
  [%expect {| "ab" |}];
  test "=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=";
  [%expect {| "ab" |}];
  test "=?ISO-8859-1?Q?a?=\n       =?ISO-8859-1?Q?b?=";
  [%expect {| "ab" |}];
  test "=?ISO-8859-1?Q?a_b?=";
  [%expect {| "a b" |}];
  test "=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?_b?=";
  [%expect {| "a b" |}];
  test " =?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>";
  [%expect {| " Keith Moore <moore@cs.utk.edu>" |}];
  test " =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>";
  [%expect {| " Keld J\248rn Simonsen <keld@dkuug.dk>" |}];
  test " =?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>";
  [%expect {| " Andr\233 Pirard <PIRARD@vm1.ulg.ac.be>" |}];
  test
    "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=\n\
     =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=";
  [%expect {| "If you can read this you understand the example." |}];
  (* Big5 example *)
  test "=?Big5?B?pl7C0DogW0V4dGVybmFsXSBSZTogPDxKYW5lc3RyZWV0Pj4gVW5tYXRjaGVk?=";
  [%expect {| "\166^\194\208: [External] Re: <<Janestreet>> Unmatched" |}];
  (* a UTF8 Example *)
  test "=?UTF-8?B?SGkgVGhlcmVcIQo=?=";
  [%expect {| "Hi There\\!\n" |}];
  (* mixed encodings *)
  test "Hello =?US-ASCII?Q?ascii?= =?ISO-8859-1?B?YmluYXJ5?= =?UTF-8?Q?world?=";
  [%expect {| "Hello asciibinaryworld" |}];
  test "Ocaml =?UTF-8?B?8J+Qqg==?= Code";
  [%expect {| "Ocaml \240\159\144\170 Code" |}]
;;

let%expect_test "decode-with-charset" =
  let test str =
    let decoded = decode_with_charset str |> Or_error.ok_exn in
    print_s
      [%sexp
        (decoded
         : [ `Encoded of
             [ `Ascii
             | `Big5
             | `GB2312
             | `Latin1
             | `Latin2
             | `Utf8
             | `Windows1252
             | `KS_C_5601_1987
             ]
             * string
           | `Plain of string
           ]
             list)]
  in
  test "hello = there?!?";
  [%expect {| ((Plain hello) (Plain " ") (Plain =) (Plain " ") (Plain there?!?)) |}];
  test "hello\n there\n\tagain\n   my\nfriend";
  [%expect
    {|
    ((Plain hello) (Plain " ") (Plain there) (Plain "\t") (Plain again)
     (Plain "   ") (Plain my) (Plain "\n") (Plain friend))
    |}];
  (* Some test vectors from: https://tools.ietf.org/html/rfc2047 *)
  test "=?ISO-8859-1?Q?a?=";
  [%expect {| ((Encoded (Latin1 a))) |}];
  test "=?ISO-8859-1?Q?a?= b";
  [%expect {| ((Encoded (Latin1 a)) (Plain " ") (Plain b)) |}];
  test "=?ISO-8859-1?Q?a?= =?ISO-8859-1?Q?b?=";
  [%expect {| ((Encoded (Latin1 a)) (Encoded (Latin1 b))) |}];
  test "=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=";
  [%expect {| ((Encoded (Latin1 a)) (Encoded (Latin1 b))) |}];
  test "=?ISO-8859-1?Q?a?=\n       =?ISO-8859-1?Q?b?=";
  [%expect {| ((Encoded (Latin1 a)) (Encoded (Latin1 b))) |}];
  test "=?ISO-8859-1?Q?a_b?=";
  [%expect {| ((Encoded (Latin1 "a b"))) |}];
  test "=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?_b?=";
  [%expect {| ((Encoded (Latin1 a)) (Encoded (Latin1 " b"))) |}];
  test " =?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>";
  [%expect
    {|
    ((Plain " ") (Encoded (Ascii "Keith Moore")) (Plain " ")
     (Plain <moore@cs.utk.edu>))
    |}];
  test " =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>";
  [%expect
    {|
    ((Plain " ") (Encoded (Latin1 "Keld J\248rn Simonsen")) (Plain " ")
     (Plain <keld@dkuug.dk>))
    |}];
  test " =?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>";
  [%expect
    {|
    ((Plain " ") (Encoded (Latin1 "Andr\233")) (Plain " ") (Plain Pirard)
     (Plain " ") (Plain <PIRARD@vm1.ulg.ac.be>))
    |}];
  test
    "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=\n\
     =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=";
  [%expect
    {|
    ((Encoded (Latin1 "If you can read this yo"))
     (Encoded (Latin2 "u understand the example.")))
    |}];
  (* Big5 example *)
  test "=?Big5?B?pl7C0DogW0V4dGVybmFsXSBSZTogPDxKYW5lc3RyZWV0Pj4gVW5tYXRjaGVk?=";
  [%expect
    {| ((Encoded (Big5 "\166^\194\208: [External] Re: <<Janestreet>> Unmatched"))) |}];
  (* a UTF8 Example *)
  test "=?UTF-8?B?SGkgVGhlcmVcIQo=?=";
  [%expect {| ((Encoded (Utf8 "Hi There\\!\n"))) |}];
  (* mixed encodings *)
  test "Hello =?US-ASCII?Q?ascii?= =?ISO-8859-1?B?YmluYXJ5?= =?UTF-8?Q?world?=";
  [%expect
    {|
    ((Plain Hello) (Plain " ") (Encoded (Ascii ascii)) (Encoded (Latin1 binary))
     (Encoded (Utf8 world)))
    |}];
  test "Ocaml =?UTF-8?B?8J+Qqg==?= Code";
  [%expect
    {|
    ((Plain Ocaml) (Plain " ") (Encoded (Utf8 "\240\159\144\170")) (Plain " ")
     (Plain Code))
    |}]
;;

let%expect_test _ =
  let test str = printf "%S\n" (decode str |> Or_error.ok_exn) in
  (* The RFC is pretty clear in saying that spaces inside the encode word are not
     supported. Yet we've seen some incorrectly encoded headers in the wild.

     Adding this test to document that the current behaviour is correct.
  *)
  test
    "=?UTF-8?Q?encoded word with spaces ?= =?UTF-8?Q?and new\n\
     lines?= =?UTF-8?Q?should=20not=20be=20decoded?=";
  [%expect
    {| "=?UTF-8?Q?encoded word with spaces ?= =?UTF-8?Q?and new\nlines?= should not be decoded" |}]
;;
