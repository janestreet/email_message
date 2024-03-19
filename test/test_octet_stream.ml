open! Core
open Async
open Email_message
open Octet_stream

let test ~encoding plaintext =
  let coded = encode ~encoding (Bigstring_shared.of_string plaintext) in
  printf "%s" (Octet_stream.encoded_contents_string coded);
  let decoded = decode coded |> Option.value_exn |> Bigstring_shared.to_string in
  [%test_result: string] decoded ~expect:plaintext
;;

let%expect_test "base64" =
  let test = test ~encoding:`Base64 in
  test "any carnal pleasure.";
  [%expect {| YW55IGNhcm5hbCBwbGVhc3VyZS4= |}];
  test "any carnal pleasure";
  [%expect {| YW55IGNhcm5hbCBwbGVhc3VyZQ== |}];
  test "any carnal pleasur";
  [%expect {| YW55IGNhcm5hbCBwbGVhc3Vy |}];
  test "any carnal pleasu";
  [%expect {| YW55IGNhcm5hbCBwbGVhc3U= |}];
  test "any carnal pleas";
  [%expect {| YW55IGNhcm5hbCBwbGVhcw== |}];
  test
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \
     exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \
     irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \
     pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia \
     deserunt mollit anim id est laborum.";
  [%expect
    {|
    TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwg
    c2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWdu
    YSBhbGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0
    aW9uIHVsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1
    YXQuIER1aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2
    ZWxpdCBlc3NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRl
    dXIgc2ludCBvY2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1
    aSBvZmZpY2lhIGRlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg==
    |}];
  return ()
;;

let%expect_test "quoted printable" =
  let test = test ~encoding:`Quoted_printable in
  test
    "If you believe that truth=beauty, then surely mathematics is the most beautiful \
     branch of philosophy.";
  [%expect
    {|
    If you believe that truth=3Dbeauty, then surely mathematics is the most =
    beautiful branch of philosophy.
    |}];
  test "\000\001\002a\003    \n";
  [%expect {| =00=01=02a=03   =20 |}];
  test "\000\001\002a\003    \n";
  [%expect {| =00=01=02a=03   =20 |}];
  test "\000\001\002a\003    \n";
  [%expect {| =00=01=02a=03   =20 |}];
  test "\000\001\002a\003    \n";
  [%expect {| =00=01=02a=03   =20 |}];
  test "This text is fairly long and should be wrapped by a conforming implementation.";
  [%expect
    {|
    This text is fairly long and should be wrapped by a conforming =
    implementation.
    |}];
  test
    "123456789A123456789B123456789C123456789D123456789E123456789F123456789G123456789H123456789I";
  [%expect
    {|
    123456789A123456789B123456789C123456789D123456789E123456789F123456789G12345=
    6789H123456789I
    |}];
  test
    "123456789A123456789B123456789C123456789D123456789E123456789F123456789G=23456789H123456789I";
  [%expect
    {|
    123456789A123456789B123456789C123456789D123456789E123456789F123456789G=3D23=
    456789H123456789I
    |}];
  return ()
;;
