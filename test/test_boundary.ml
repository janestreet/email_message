open! Core
open Async
open Email_message
open Email_message.Private
open Expect_test_helpers_core
open Boundary


let boundary = of_string "BOUNDARY"

let split str =
  let bs = Bigstring_shared.to_string in
  let prologue, parts, epilogue = split boundary (Bigstring_shared.of_string str) in
  let prologue = Option.map prologue ~f:bs in
  let parts = List.map parts ~f:bs in
  let epilogue = Option.map epilogue ~f:bs in
  print_s
    [%message
      "" (prologue : string option) (parts : string list) (epilogue : string option)]
;;

let join (prologue, parts, epilogue) =
  let prologue = Option.map prologue ~f:Bigstring_shared.of_string in
  let epilogue = Option.map epilogue ~f:Bigstring_shared.of_string in
  let parts = List.map parts ~f:String_monoid.of_string in
  let joined = join_without_checking_for_conflicts ?prologue ~parts ?epilogue boundary in
  (* Expect tests ignore leading and trailing whitespace (and common indentation)
     on both the 'expected' and 'actual' outputs. Wrapping in '######'s ensures
     that no whitespace is stripped. *)
  printf "######\n%s\n######" (String_monoid.to_string joined)
;;

let%expect_test "split" =
  (* Simple tests with no prologue or epilogue *)
  split "--BOUNDARY\n\n--BOUNDARY--";
  [%expect {| ((prologue ()) (parts ("")) (epilogue ())) |}];
  split "--BOUNDARY\nP\n--BOUNDARY--";
  [%expect {| ((prologue ()) (parts (P)) (epilogue ())) |}];
  split "--BOUNDARY\r\nP\r\n--BOUNDARY--";
  [%expect {| ((prologue ()) (parts (P)) (epilogue ())) |}];
  split "--BOUNDARY\nP\n--BOUNDARY\nQ\n--BOUNDARY--";
  [%expect {| ((prologue ()) (parts (P Q)) (epilogue ())) |}];
  (* Prologue and epilogue *)
  split "A\n--BOUNDARY\nP\n--BOUNDARY--";
  [%expect {|
      ((prologue (A))
       (parts    (P))
       (epilogue ())) |}];
  split "--BOUNDARY\nP\n--BOUNDARY--\nB";
  [%expect {|
      ((prologue ())
       (parts    (P))
       (epilogue ("\nB"))) |}];
  split "A\n--BOUNDARY\nP\n--BOUNDARY--\nB";
  [%expect {|
      ((prologue (A))
       (parts    (P))
       (epilogue ("\nB"))) |}];
  split "A\r\n--BOUNDARY\r\nP\r\n--BOUNDARY--\r\nB";
  [%expect {|
      ((prologue (A))
       (parts    (P))
       (epilogue ("\r\nB"))) |}];
  (* Preserve extra whitespace *)
  split "\nA\n\n--BOUNDARY\n\nP\n\n--BOUNDARY--\nB\n";
  [%expect
    {|
      ((prologue ("\nA\n"))
       (parts    ("\nP\n"))
       (epilogue ("\nB\n"))) |}];
  (* Whitespace padding on boundary line *)
  split "--BOUNDARY \nB\n--BOUNDARY--";
  [%expect
    {|
      ((prologue ("--BOUNDARY \nB"))
       (parts    ())
       (epilogue ())) |}];
  (* Content with something that looks like a boundary *)
  split "--BOUNDARY\nnot a --BOUNDARY\nnot a =\n--BOUNDARY either\n--BOUNDARY--";
  [%expect
    {|
      ((prologue ())
       (parts ("not a --BOUNDARY\nnot a =\n--BOUNDARY either"))
       (epilogue ())) |}];
  return ()
;;

let%expect_test "join" =

  (* Simple tests with no prologue or epilogue *)
  join (None, [ "" ], None);
  [%expect
    {|
        ######
        --BOUNDARY

        --BOUNDARY--
        ######
        |}];
  let%bind () =
    join (None, [ "P" ], None);
    [%expect
      {|
        ######
        --BOUNDARY
        P
        --BOUNDARY--
        ######
        |}];
    return ()
  in
  let%bind () =
    join (None, [ "P"; "Q" ], None);
    [%expect
      {|
        ######
        --BOUNDARY
        P
        --BOUNDARY
        Q
        --BOUNDARY--
        ######
        |}];
    return ()
  in
  (* Prologue and epilogue *)
  join (Some "A", [ "P" ], None);
  [%expect
    {|
        ######
        A
        --BOUNDARY
        P
        --BOUNDARY--
        ######
        |}];
  join (None, [ "P" ], Some "\nB");
  [%expect
    {|
        ######
        --BOUNDARY
        P
        --BOUNDARY--
        B
        ######
        |}];
  join (Some "A", [ "P" ], Some "\nB");
  [%expect
    {|
        ######
        A
        --BOUNDARY
        P
        --BOUNDARY--
        B
        ######
        |}];
  (* Preserve extra whitespace *)
  join (Some "\nA\n", [ "\nP\n" ], Some "\nB\n");
  [%expect
    {|
        ######

        A

        --BOUNDARY

        P

        --BOUNDARY--
        B

        ######
        |}];
  return ()
;;

module Non_compliant = struct
  (* The following tests document undefined behavior. *)

  let%expect_test "non-compliant [split]" =
    (* Parsing of weird and malformed data into a sensible form *)
    split "--BOUNDARY\n--BOUNDARY--";
    [%expect {|
        ((prologue ()) (parts ("")) (epilogue ()))
        |}];
    split "";
    [%expect
      {|
        ((prologue (""))
         (parts    ())
         (epilogue ()))
        |}];
    split "\n";
    [%expect
      {|
        ((prologue ("\n"))
         (parts    ())
         (epilogue ()))
        |}];
    (* Missing boundary markers *)
    split "A\n--BOUNDARY--\nB";
    [%expect {|
        ((prologue (A)) (parts ()) (epilogue ("\nB")))
        |}];
    split "--BOUNDARY--\nB";
    [%expect {|
        ((prologue ("")) (parts ()) (epilogue ("\nB")))
        |}];
    split "--BOUNDARY--\n";
    [%expect {|
        ((prologue ("")) (parts ()) (epilogue ("\n")))
        |}];
    split "--BOUNDARY--";
    [%expect
      {|
        ((prologue (""))
         (parts    ())
         (epilogue ()))
        |}];
    split "A\n--BOUNDARY--";
    [%expect
      {|
        ((prologue (A))
         (parts    ())
         (epilogue ()))
        |}];
    return ()
  ;;

  let%expect_test "non-compliant [join]" =
    join (Some "A", [], Some "\nB");
    [%expect {|
        ######
        A
        B
        ######
      |}];
    join (None, [], Some "\nB");
    [%expect {|
        ######

        B
        ######
      |}];
    join (None, [], None);
    [%expect {|
        ######


        ######
      |}];
    join (Some "A", [], None);
    [%expect {|
        ######
        A
        ######
      |}];
    return ()
  ;;
end
