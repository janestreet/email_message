open Core
open Email_message.Private.Encoded_word

let%expect_test _ =
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
  (* a UTF8 Example *)
  test "=?UTF-8?B?SGkgVGhlcmVcIQo=?=";
  [%expect {| "Hi There\\!\n" |}]
;;

let%expect_test _ =
  let test str = printf "%S\n" (decode str |> Or_error.ok_exn) in
  (* The RFC is pretty clear in saying that spaces inside the encode word are not supported.
     Yet we've seen some incorrectly encoded headers in the wild.

     Adding this test to document that the current behaviour is correct.
  *)
  test
    "=?UTF-8?Q?encoded word with spaces ?= =?UTF-8?Q?and new\n\
     lines?= =?UTF-8?Q?should=20not=20be=20decoded?=";
  [%expect
    {| "=?UTF-8?Q?encoded word with spaces ?= =?UTF-8?Q?and new\nlines?= should not be decoded" |}]
;;
