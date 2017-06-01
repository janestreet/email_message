open! Core
open Async
open Expect_test_helpers

open Email_message.Email_address

let%test_unit "compare" =
  let equal a b =
    let result = compare (of_string_exn a) (of_string_exn b) in
    [%test_result : int] result ~expect:0
  in
  equal
    "foobar <foo@bar.com>"
    "foo@bar.com"
;;

let print_addr addr =
    let prefix = prefix addr in
    let local_part = local_part addr in
    let domain = domain addr in
    print_s [%message
      ""
        (prefix : string option)
        (local_part : string)
        (domain : string option)
    ]
;;

let%expect_test "parse one" =
  let parse str = print_addr (of_string_exn str) in
  parse "local";
  let%bind () =
    [%expect {| ((prefix ()) (local_part local) (domain ())) |}]
  in
  parse "<local>";
  let%bind () =
    [%expect {| ((prefix ("")) (local_part local) (domain ())) |}]
  in
  parse " local@janestreet.com ";
  let%bind () =
    [%expect {| ((prefix ()) (local_part local) (domain (janestreet.com))) |}]
  in
  parse " <local@janestreet.com> ";
  let%bind () =
    [%expect {| ((prefix ("")) (local_part local) (domain (janestreet.com))) |}]
  in
  parse " John Doe <local> ";
  let%bind () =
    [%expect {| ((prefix ("John Doe ")) (local_part local) (domain ())) |}]
  in
  parse " John Doe <local@janestreet.com> ";
  let%bind () =
    [%expect {| ((prefix ("John Doe ")) (local_part local) (domain (janestreet.com))) |}]
  in
  parse " \"Doe, John\" <local@janestreet.com> ";
  let%bind () =
    [%expect {| ((prefix ("\"Doe, John\" ")) (local_part local) (domain (janestreet.com))) |}]
  in
  show_raise (fun () -> parse "'local@janestreet.com'");
  let%bind () =
    [%expect {| (raised (Failure "Email_address: unexpected character, expecting ','")) |}]
  in
  show_raise (fun () -> parse "\"local@janestreet.com\"");
  let%bind () =
    [%expect {| (raised (Failure "Email_address: unexpected character, expecting ','")) |}]
  in
  parse "\"local@janestreet.com\" <local@janestreet.com>";
  let%bind () =
    [%expect {|
      ((prefix ("\"local@janestreet.com\" "))
       (local_part local)
       (domain (janestreet.com))) |}]
  in
  (* Escaping *)
  show_raise (fun () -> parse "\"\\\"Description within quotes\\\"\"<local@janestreet.com>");
  let%bind () =
    [%expect {| (raised (Failure "lexing: empty token")) |}]
  in
  show_raise (fun () -> parse "local\\@@janestreet.com");
  let%bind () =
    [%expect {| (raised (Failure "lexing: empty token")) |}]
  in
  return ()
;;

let%expect_test "parse many" =
  let parse str = List.iter (list_of_string_exn str) ~f:print_addr in
  parse "";
  let%bind () =
    [%expect {| |}]
  in
  parse " ";
  let%bind () =
    [%expect {| |}]
  in
  show_raise (fun () -> parse ",");
  let%bind () =
    [%expect {| (raised (Failure "Email_address: unexpected ','")) |}]
  in
  show_raise (fun () -> parse ", local@janestreet.com,");
  let%bind () =
    [%expect {| (raised (Failure "Email_address: unexpected ','")) |}]
  in
  show_raise (fun () -> parse "local@janestreet.com, ,local@janestreet.com,");
  let%bind () =
    [%expect {|
      (raised (Failure "Email_address: unexpected ','")) |}]
  in
  parse " \"Doe, John\" <local@janestreet.com>,\n\t\
         \"Doe, Johnny\" <local@janestreet.com> ";
  let%bind () =
    [%expect {|
    ((prefix ("\"Doe, John\" ")) (local_part local) (domain (janestreet.com)))
    ((prefix ("\"Doe, Johnny\" ")) (local_part local) (domain (janestreet.com))) |}]
  in
  parse "x@y.com, \"a@b.com\" <\"mailto:a\"@b.com>";
  let%bind () =
    [%expect {|
    ((prefix ()) (local_part x) (domain (y.com)))
    ((prefix ("\"a@b.com\" ")) (local_part "\"mailto:a\"") (domain (b.com))) |}]
  in
  show_raise (fun () -> parse "mailnull@janestreet.com (Cron Daemon)");
  let%bind () =
    [%expect {|
    (raised (Failure "lexing: empty token")) |}]
  in
  show_raise (fun () -> parse "a@b.com<a@b.com>");
  let%bind () =
    [%expect {| (raised (Failure "Email_address: unexpected character, expecting ','")) |}]
  in
  show_raise (fun () -> parse "a@b.com <a@b.com>");
  let%bind () =
    [%expect {| (raised (Failure "lexing: empty token")) |}]
  in
  show_raise (fun () -> parse "a@@b.com");
  let%bind () =
    [%expect {| (raised (Failure "lexing: empty token")) |}]
  in
  return ()
;;
