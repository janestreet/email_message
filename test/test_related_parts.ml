open! Core
open Async
open Email_message

let test_email =
  (* This is the test email from https://tools.ietf.org/html/rfc2392 except I had to
     remove the "type=Text/HTML" from the top-most "Content-Type" header. *)
  String.strip
    {|
From: foo1@bar.net
To: foo2@bar.net
Subject: A simple example
Mime-Version: 1.0
Content-Type: multipart/related; boundary="boundary-example-1"
--boundary-example 1
Content-Type: Text/HTML; charset=US-ASCII

to the other body part, for example through a statement such as:
<IMG SRC="cid:foo4*foo1@bar.net" ALT="IETF logo">

--boundary-example-1
Content-ID: <foo4*foo1@bar.net>
Content-Type: IMAGE/GIF
Content-Transfer-Encoding: BASE64

R0lGODlhGAGgAPEAAP/////ZRaCgoAAAACH+PUNvcHlyaWdodCAoQykgMTk5
NSBJRVRGLiBVbmF1dGhvcml6ZWQgZHVwbGljYXRpb24gcHJvaGliaXRlZC4A
etc...

--boundary-example-1--
|}
;;

let%expect_test _ =
  let email = Email.of_string test_email in
  Email.Simple.all_related_parts email
  |> [%sexp_of: (string * Email.Simple.Content.t) list]
  |> print_s;
  [%expect
    {|
    ((foo4*foo1@bar.net
      ((headers
        ((Content-ID " <foo4*foo1@bar.net>") (Content-Type " IMAGE/GIF")
         (Content-Transfer-Encoding " BASE64")))
       (raw_content
        ( "R0lGODlhGAGgAPEAAP/////ZRaCgoAAAACH+PUNvcHlyaWdodCAoQykgMTk5\
         \nNSBJRVRGLiBVbmF1dGhvcml6ZWQgZHVwbGljYXRpb24gcHJvaGliaXRlZC4A\
         \netc...\
         \n"))))) |}];
  return ()
;;
