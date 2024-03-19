open! Core
open Async
open Email_message
open Expect_test_helpers_async

let save ?eol_except_raw_content str =
  let email = Email.of_string str in
  with_temp_dir (fun tmpdir ->
    let path = tmpdir ^/ "email" in
    let%bind () = Email.save ?eol_except_raw_content email path in
    let%bind on_disk = Reader.file_contents path in
    String.concat_map on_disk ~f:(fun c ->
      if Char.equal c '\r' then "\\r" else String.of_char c)
    |> printf "%s";
    Deferred.unit)
;;

let%expect_test "save" =
  let email_str = "A: B\nC: D\n\nLine 1\nLine 2" in
  let%bind () = save email_str in
  [%expect {|
    A: B
    C: D

    Line 1
    Line 2
    |}];
  let%bind () = save ~eol_except_raw_content:`CRLF email_str in
  [%expect {|
    A: B\r
    C: D\r
    \r
    Line 1
    Line 2
    |}];
  Deferred.unit
;;
