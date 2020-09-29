open Core
open Async
open Expect_test_helpers_core
open Email_address

let%test_unit "check comparison" =
  let open! Int.Replace_polymorphic_compare in
  (* Ignore prefixes *)
  [ ( create ~prefix:"foo" ~domain:"bar.com" "A"
    , create ~prefix:"foo" ~domain:"bar.com" "A"
    , true )
  ; ( create ~prefix:"foo" ~domain:"bar.com" "A"
    , create ~prefix:"bar" ~domain:"bar.com" "A"
    , true )
  ; create ~prefix:"foo" ~domain:"bar.com" "A", create ~domain:"bar.com" "A", true
  ; ( create ~prefix:"A, B" ~domain:"X.COM" "ab"
    , create ~prefix:"A, B" ~domain:"X.COM" "ab"
    , true )
  (* Case-insensitive domain *)
  ; create ~domain:"BAR.com" "A", create ~domain:"bar.com" "A", true
  ; create ~domain:"foo.com" "A", create ~domain:"bar.com" "A", false
  (* Case-sensitive local part *)
  ; create "A", create "a", false
  ]
  |> Core_kernel.List.iter ~f:(fun (l, r, equal) ->
    [%test_result: bool] ~expect:equal (Email_address.compare l r = 0))
;;

let%test_unit "check example email list" =
  let open! Int.Replace_polymorphic_compare in
  [%test_result: Email_address.t list Or_error.t]
    (list_of_string {|"A, B" <ab@x.com>, "C, D" <cd@x.com> |})
    ~expect:
      (Ok
         [ create ~prefix:"A, B" ~domain:"X.COM" "ab"
         ; create ~prefix:"C, D" ~domain:"x.com" "cd"
         ])
;;

let%test_unit "compare" =
  let equal a b =
    let result = compare (of_string_exn a) (of_string_exn b) in
    [%test_result: int] result ~expect:0
  in
  equal "foobar <foo@bar.com>" "foo@bar.com"
;;

let print_addr addr =
  let prefix = prefix addr in
  let local_part = local_part addr in
  let domain = domain addr in
  print_s
    [%message
      "" (prefix : string option) (local_part : string) (domain : Domain.t option)]
;;

let%expect_test "parse one" =
  let parse str = print_addr (of_string_exn str) in
  show_raise (fun () -> parse "  ");
  [%expect
    {|
      (raised (
        "Failed to parse email address"
        (error     "email > local_part: count_while1")
        (input_str "  "))) |}];
  parse "local";
  [%expect {| ((prefix ()) (local_part local) (domain ())) |}];
  parse "<local>";
  [%expect {| ((prefix ("")) (local_part local) (domain ())) |}];
  parse " local@janestreet.com ";
  [%expect {| ((prefix ()) (local_part local) (domain (janestreet.com))) |}];
  parse " <local@janestreet.com> ";
  [%expect {| ((prefix ("")) (local_part local) (domain (janestreet.com))) |}];
  parse " John Doe <local> ";
  [%expect {| ((prefix ("John Doe ")) (local_part local) (domain ())) |}];
  parse " John Doe <local@janestreet.com> ";
  [%expect {| ((prefix ("John Doe ")) (local_part local) (domain (janestreet.com))) |}];
  parse " \"Doe, John\" <local@janestreet.com> ";
  [%expect
    {| ((prefix ("\"Doe, John\" ")) (local_part local) (domain (janestreet.com))) |}];
  show_raise (fun () -> parse "'local@janestreet.com'");
  [%expect
    {|
      (raised (
        "Failed to parse email address"
        (error     ": end_of_input")
        (input_str 'local@janestreet.com'))) |}];
  show_raise (fun () -> parse "\"local@janestreet.com\"");
  [%expect
    {|
      (raised (
        "Failed to parse email address"
        (error     ": end_of_input")
        (input_str "\"local@janestreet.com\""))) |}];
  parse "\"local@janestreet.com\" <local@janestreet.com>";
  [%expect
    {|
      ((prefix ("\"local@janestreet.com\" "))
       (local_part local)
       (domain (janestreet.com))) |}];
  show_raise (fun () -> parse "local@janestreet.com, local@janestreet.com");
  [%expect
    {|
      (raised (
        "Failed to parse email address"
        (error ": end_of_input")
        (input_str "local@janestreet.com, local@janestreet.com"))) |}];
  (* Escaping *)
  show_raise (fun () ->
    parse "\"\\\"Description within quotes\\\"\"<local@janestreet.com>");
  [%expect
    {|
      (raised (
        "Failed to parse email address"
        (error ": end_of_input")
        (input_str "\"\\\"Description within quotes\\\"\"<local@janestreet.com>"))) |}];
  show_raise (fun () -> parse "local\\@@janestreet.com");
  [%expect
    {|
      (raised (
        "Failed to parse email address"
        (error     ": end_of_input")
        (input_str "local\\@@janestreet.com"))) |}];
  return ()
;;

let%expect_test "parse many" =
  let parse str = List.iter (list_of_string_exn str) ~f:print_addr in
  parse "";
  [%expect {| |}];
  parse " ";
  [%expect {| |}];
  show_raise (fun () -> parse ",");
  [%expect
    {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str ,))) |}];
  show_raise (fun () -> parse ", local@janestreet.com,");
  [%expect
    {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str ", local@janestreet.com,"))) |}];
  show_raise (fun () -> parse "local@janestreet.com, ,local@janestreet.com,");
  [%expect
    {|
      (raised (
        "Failed to parse email address(es)"
        (error ": end_of_input")
        (input_str "local@janestreet.com, ,local@janestreet.com,"))) |}];
  parse
    " \"Doe, John\" <local@janestreet.com>,\n\t\"Doe, Johnny\" <local@janestreet.com> ";
  [%expect
    {|
    ((prefix ("\"Doe, John\" ")) (local_part local) (domain (janestreet.com)))
    ((prefix ("\"Doe, Johnny\" ")) (local_part local) (domain (janestreet.com))) |}];
  parse "x@y.com, \"a@b.com\" <\"mailto:a\"@b.com>";
  [%expect
    {|
    ((prefix ()) (local_part x) (domain (y.com)))
    ((prefix ("\"a@b.com\" ")) (local_part "\"mailto:a\"") (domain (b.com))) |}];
  show_raise (fun () -> parse "mailnull@janestreet.com (Cron Daemon)");
  [%expect
    {|
    (raised (
      "Failed to parse email address(es)"
      (error     ": end_of_input")
      (input_str "mailnull@janestreet.com (Cron Daemon)"))) |}];
  show_raise (fun () -> parse "a@b.com<a@b.com>");
  [%expect
    {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str a@b.com<a@b.com>))) |}];
  show_raise (fun () -> parse "a@b.com <a@b.com>");
  [%expect
    {|
      (raised (
        "Failed to parse email address(es)"
        (error     ": end_of_input")
        (input_str "a@b.com <a@b.com>"))) |}];
  show_raise (fun () -> parse "a@@b.com");
  [%expect
    {|
    (raised (
      "Failed to parse email address(es)"
      (error     ": end_of_input")
      (input_str a@@b.com))) |}];
  parse "x@y.com, \"a@b.com\" <\"mailto:a\"@b.com>";
  [%expect
    {|
    ((prefix ()) (local_part x) (domain (y.com)))
    ((prefix ("\"a@b.com\" ")) (local_part "\"mailto:a\"") (domain (b.com))) |}];
  parse {|John Doe <jdoe@machine.example>|};
  [%expect {| ((prefix ("John Doe ")) (local_part jdoe) (domain (machine.example))) |}];
  parse {|"Joe Q. Public" <john.q.public@example.com>|};
  [%expect
    {|
    ((prefix ("\"Joe Q. Public\" "))
     (local_part john.q.public)
     (domain (example.com))) |}];
  parse {|Joe Q. Public <john.q.public@example.com>|};
  [%expect
    {|
    ((prefix ("Joe Q. Public "))
     (local_part john.q.public)
     (domain (example.com))) |}];
  parse {|"John (middle) Doe" <jdoe@machine.example>|};
  [%expect
    {|
    ((prefix ("\"John (middle) Doe\" "))
     (local_part jdoe)
     (domain (machine.example))) |}];
  parse {|John (middle) Doe <jdoe@machine.example>|};
  [%expect
    {| ((prefix ("John (middle) Doe ")) (local_part jdoe) (domain (machine.example))) |}];
  parse {|"John <middle> Doe" <jdoe@machine.example>|};
  [%expect
    {|
    ((prefix ("\"John <middle> Doe\" "))
     (local_part jdoe)
     (domain (machine.example))) |}];
  parse {|"John; Doe" <jdoe@machine.example>|};
  [%expect
    {| ((prefix ("\"John; Doe\" ")) (local_part jdoe) (domain (machine.example))) |}];
  parse {|"John, Doe" <jdoe@machine.example>|};
  [%expect
    {| ((prefix ("\"John, Doe\" ")) (local_part jdoe) (domain (machine.example))) |}];
  parse {|<jdoe@machine.example>, foo@bar|};
  [%expect
    {|
    ((prefix ("")) (local_part jdoe) (domain (machine.example)))
    ((prefix ()) (local_part foo) (domain (bar))) |}];
  parse {|Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>|};
  [%expect
    {|
    ((prefix ("Mary Smith ")) (local_part mary) (domain (x.test)))
    ((prefix ()) (local_part jdoe) (domain (example.org)))
    ((prefix ("Who? ")) (local_part one) (domain (y.test))) |}];
  show_raise (fun () ->
    parse {|<boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net>|});
  [%expect
    {|
    (raised (
      "Failed to parse email address(es)"
      (error ": end_of_input")
      (input_str
       "<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>"))) |}];
  parse {|"" <emptystring@example.com>|};
  [%expect {| ((prefix ("\"\" ")) (local_part emptystring) (domain (example.com))) |}];
  return ()
;;
