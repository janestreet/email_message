open! Core
open Async
open Email_message
open Email.Simple

let%expect_test "[Expert.create_raw] Message-Id handling" =
  let message_id_headers ~message_id_in_extra_headers ~id_supplied =
    let extra_headers =
      match message_id_in_extra_headers with
      | None -> []
      | Some id -> [ "Message-Id", id ]
    in
    let headers =
      Expert.create_raw
        ~to_:[ "foo@bar.com" ]
        ~subject:"test"
        ?id:id_supplied
        ~extra_headers
        (Content.text_utf8 "")
      |> Email.headers
    in
    Email_headers.find_all headers "Message-Id" |> List.iter ~f:print_endline
  in
  (* Auto-generate an id if nothing is supplied *)
  message_id_headers ~message_id_in_extra_headers:None ~id_supplied:None;
  [%expect {| {AUTO-GENERATED-ID} |}];
  (* Use the supplied id *)
  message_id_headers ~message_id_in_extra_headers:(Some "ID") ~id_supplied:None;
  (* Look into extra-headers for a message-id too *)
  [%expect {| ID |}];
  message_id_headers ~message_id_in_extra_headers:None ~id_supplied:(Some "ID");
  [%expect {| ID |}];
  (* If an id is supplied in both places and they match, only show it once *)
  message_id_headers ~message_id_in_extra_headers:(Some "ID1") ~id_supplied:(Some "ID1");
  [%expect {| ID1 |}];
  (* But if they don't match, include both values *)
  message_id_headers ~message_id_in_extra_headers:(Some "ID1") ~id_supplied:(Some "ID2");
  [%expect
    {|
    ID1
    ID2
    |}];
  return ()
;;

let%expect_test "[Expert.content]" =
  let content ~normalize_headers ~encoding ~extra_headers str =
    let result =
      Expert.content ~normalize_headers ~encoding ~extra_headers str |> Email.to_string
    in
    printf "%s" result
  in
  content
    ~normalize_headers:`None
    ~encoding:`Quoted_printable
    ~extra_headers:[ "header1", "value1"; "header2", "value2" ]
    "x";
  [%expect
    {|
    Content-Transfer-Encoding:quoted-printable
    header1:value1
    header2:value2

    x
    |}];
  content
    ~normalize_headers:`Whitespace
    ~encoding:`Quoted_printable
    ~extra_headers:[]
    "x\n";
  [%expect
    {|
    Content-Transfer-Encoding: quoted-printable

    x
    |}];
  return ()
;;

let replacement = (Content.text_utf8 "<REPLACED>" :> Email.t)

let parse_attachments ~replace_attachment s =
  let email = Email.of_string s in
  let attachments =
    List.map (all_attachments email) ~f:(fun attachment ->
      let id = Attachment.id attachment in
      let raw_data =
        Attachment.raw_data attachment |> ok_exn |> Bigstring_shared.to_string
      in
      id, raw_data)
  in
  let stripped =
    map_attachments email ~f:(fun attachment ->
      if replace_attachment ~name:(Attachment.filename attachment)
      then `Replace replacement
      else `Keep)
  in
  printf
    !"%s"
    (Sexp.to_string_hum
       [%message "" (attachments : (Attachment.Id.t * string) list) (stripped : Email.t)])
;;

let parse_attachments' l = parse_attachments (String.concat l ~sep:"\n")

let%expect_test "[all_attachments] and [map_attachments]" =
  parse_attachments'
    ~replace_attachment:(fun ~name:_ -> true)
    [ "Content-Type: multipart/mixed; boundary=BOUNDARY1"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: multipart/alternative; boundary=BOUNDARY2"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/plain; charset=UTF-8"
    ; ""
    ; "Simple body"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/html; charset=UTF-8"
    ; ""
    ; "<div>Simple body</div>"
    ; ""
    ; "--BOUNDARY2--"
    ; "--BOUNDARY1"
    ; "Content-Type: text/plain; charset=US-ASCII; name=\"attachment.txt\""
    ; "Content-Disposition: attachment; filename=\"attachment.txt\""
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "Zm9v"
    ; "--BOUNDARY1--"
    ];
  [%expect
    {|
    ((attachments ((((filename attachment.txt) (path (1))) foo)))
     (stripped
      ((headers ((Content-Type " multipart/mixed; boundary=BOUNDARY1")))
       (raw_content
        ( "--BOUNDARY1\
         \nContent-Type: multipart/alternative; boundary=BOUNDARY2\
         \n\
         \n--BOUNDARY2\
         \nContent-Type: text/plain; charset=UTF-8\
         \n\
         \nSimple body\
         \n\
         \n--BOUNDARY2\
         \nContent-Type: text/html; charset=UTF-8\
         \n\
         \n<div>Simple body</div>\
         \n\
         \n--BOUNDARY2--\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1--")))))
    |}];
  (* - parse into "multipart/digest" parts
     - the "message/rfc822" content type is optional *)
  parse_attachments'
    ~replace_attachment:(fun ~name:_ -> true)
    [ "Content-Type: multipart/digest; boundary=BOUNDARY"
    ; ""
    ; "--BOUNDARY"
    ; ""
    ; "Content-Type: multipart/mixed; boundary=BOUNDARY1"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: multipart/alternative; boundary=BOUNDARY2"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/plain; charset=UTF-8"
    ; ""
    ; "Simple body"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/html; charset=UTF-8"
    ; ""
    ; "<div>Simple body</div>"
    ; ""
    ; "--BOUNDARY2--"
    ; "--BOUNDARY1"
    ; "Content-Type: text/plain; charset=US-ASCII; name=\"attachment.txt\""
    ; "Content-Disposition: attachment; filename=\"attachment.txt\""
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "Zm9v"
    ; "--BOUNDARY1--"
    ; "--BOUNDARY"
    ; "Content-Type:message/rfc822"
    ; ""
    ; "Subject: No content-Type in the message headers"
    ; ""
    ; ""
    ; "--BOUNDARY--"
    ];
  [%expect
    {|
    ((attachments ((((filename attachment.txt) (path (0 0 1))) foo)))
     (stripped
      ((headers
        ((Content-Type
          " multipart/digest; boundary=\"--==::BOUNDARY::000000::==--\"")))
       (raw_content
        ( "----==::BOUNDARY::000000::==--\
         \n\
         \nContent-Type: multipart/mixed; boundary=BOUNDARY1\
         \n\
         \n--BOUNDARY1\
         \nContent-Type: multipart/alternative; boundary=BOUNDARY2\
         \n\
         \n--BOUNDARY2\
         \nContent-Type: text/plain; charset=UTF-8\
         \n\
         \nSimple body\
         \n\
         \n--BOUNDARY2\
         \nContent-Type: text/html; charset=UTF-8\
         \n\
         \n<div>Simple body</div>\
         \n\
         \n--BOUNDARY2--\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1--\
         \n----==::BOUNDARY::000000::==--\
         \nContent-Type:message/rfc822\
         \n\
         \nSubject: No content-Type in the message headers\
         \n\
         \n\
         \n----==::BOUNDARY::000000::==----")))))
    |}];
  (* Look into message/rfc822 for attachments *)
  let nested_message_rfc822_email =
    [ "Content-Type: multipart/mixed; boundary=\"BOUNDARY1\""
    ; ""
    ; "--BOUNDARY1"
    ; ""
    ; "Testing"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: message/rfc822"
    ; ""
    ; "Subject: We should parse into this message for attachments"
    ; "Content-Type: multipart/mixed; boundary=\"BOUNDARY2\""
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/x-python; charset=US-ASCII; name=\"script.py\""
    ; "Content-Disposition: attachment; filename=\"script.py\""
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "VGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk"
    ; "--BOUNDARY2--"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: message/rfc822"
    ; "Content-Disposition: attachment; filename=\"rfc822-part\""
    ; ""
    ; "Content-Type: multipart/mixed; boundary=\"BOUNDARY3\""
    ; ""
    ; "--BOUNDARY3"
    ; "Content-Type: text/x-python; charset=US-ASCII; name=\"script2.py\""
    ; "Content-Disposition: attachment; filename=\"script2.py\""
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "VGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk"
    ; "--BOUNDARY3--"
    ; ""
    ; "--BOUNDARY1--"
    ]
  in
  (* There is a message/rfc822 attachment with attachments within. If we strip the
     message/rfc822 attachment, that also results in the nested attachments being removed.
  *)
  parse_attachments' ~replace_attachment:(fun ~name:_ -> true) nested_message_rfc822_email;
  [%expect
    {|
    ((attachments
      ((((filename script.py) (path (1 0 0))) "This attachment is blacklisted")
       (((filename rfc822-part) (path (2)))
         "Content-Type: multipart/mixed; boundary=\"BOUNDARY3\"\
        \n\
        \n--BOUNDARY3\
        \nContent-Type: text/x-python; charset=US-ASCII; name=\"script2.py\"\
        \nContent-Disposition: attachment; filename=\"script2.py\"\
        \nContent-Transfer-Encoding: base64\
        \n\
        \nVGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\
        \n--BOUNDARY3--\
        \n")
       (((filename script2.py) (path (2 0 0))) "This attachment is blacklisted")))
     (stripped
      ((headers ((Content-Type " multipart/mixed; boundary=\"BOUNDARY1\"")))
       (raw_content
        ( "--BOUNDARY1\
         \n\
         \nTesting\
         \n\
         \n--BOUNDARY1\
         \nContent-Type: message/rfc822\
         \n\
         \nSubject: We should parse into this message for attachments\
         \nContent-Type: multipart/mixed; boundary=\"BOUNDARY2\"\
         \n\
         \n--BOUNDARY2\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY2--\
         \n\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1--")))))
    |}];
  (* If we don't strip the message/rfc822 attachment, then we can strip only the nested
     attachments. *)
  parse_attachments'
    ~replace_attachment:(fun ~name -> not (String.equal name "rfc822-part"))
    nested_message_rfc822_email;
  [%expect
    {|
    ((attachments
      ((((filename script.py) (path (1 0 0))) "This attachment is blacklisted")
       (((filename rfc822-part) (path (2)))
         "Content-Type: multipart/mixed; boundary=\"BOUNDARY3\"\
        \n\
        \n--BOUNDARY3\
        \nContent-Type: text/x-python; charset=US-ASCII; name=\"script2.py\"\
        \nContent-Disposition: attachment; filename=\"script2.py\"\
        \nContent-Transfer-Encoding: base64\
        \n\
        \nVGhpcyBhdHRhY2htZW50IGlzIGJsYWNrbGlzdGVk\
        \n--BOUNDARY3--\
        \n")
       (((filename script2.py) (path (2 0 0))) "This attachment is blacklisted")))
     (stripped
      ((headers ((Content-Type " multipart/mixed; boundary=\"BOUNDARY1\"")))
       (raw_content
        ( "--BOUNDARY1\
         \n\
         \nTesting\
         \n\
         \n--BOUNDARY1\
         \nContent-Type: message/rfc822\
         \n\
         \nSubject: We should parse into this message for attachments\
         \nContent-Type: multipart/mixed; boundary=\"BOUNDARY2\"\
         \n\
         \n--BOUNDARY2\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY2--\
         \n\
         \n--BOUNDARY1\
         \nContent-Type: message/rfc822\
         \nContent-Disposition: attachment; filename=\"rfc822-part\"\
         \n\
         \nContent-Type: multipart/mixed; boundary=\"BOUNDARY3\"\
         \n\
         \n--BOUNDARY3\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY3--\
         \n\
         \n--BOUNDARY1--")))))
    |}];
  return ()
;;

let%expect_test "long attachment name" =
  parse_attachments'
    ~replace_attachment:(fun ~name:_ -> true)
    [ "Content-Type: multipart/mixed; boundary=BOUNDARY1"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: multipart/alternative; boundary=BOUNDARY2"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/plain; charset=UTF-8"
    ; ""
    ; "Simple body"
    ; ""
    ; "--BOUNDARY2"
    ; "Content-Type: text/html; charset=UTF-8"
    ; ""
    ; "<div>Simple body</div>"
    ; ""
    ; "--BOUNDARY2--"
    ; "--BOUNDARY1"
    ; "Content-Type: text/plain; charset=US-ASCII; name=\"attachment name"
    ; " that wraps.txt\""
    ; "Content-Disposition: attachment; filename=\"attachment name"
    ; " that wraps.txt\""
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "Zm9v"
    ; "--BOUNDARY1--"
    ];
  [%expect
    {|
    ((attachments
      ((((filename "attachment name that wraps.txt") (path (1))) foo)))
     (stripped
      ((headers ((Content-Type " multipart/mixed; boundary=BOUNDARY1")))
       (raw_content
        ( "--BOUNDARY1\
         \nContent-Type: multipart/alternative; boundary=BOUNDARY2\
         \n\
         \n--BOUNDARY2\
         \nContent-Type: text/plain; charset=UTF-8\
         \n\
         \nSimple body\
         \n\
         \n--BOUNDARY2\
         \nContent-Type: text/html; charset=UTF-8\
         \n\
         \n<div>Simple body</div>\
         \n\
         \n--BOUNDARY2--\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1--")))))
    |}];
  return ()
;;

let%expect_test "RFC2231 Support - split attachment name" =
  let rfc2231_email_with_split_attachment_name =
    [ "Content-Type: multipart/mixed; boundary=BOUNDARY1"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: text/html; charset=utf-8"
    ; "Content-Transfer-Encoding: quoted-printable"
    ; ""
    ; "<div>Simple Body</div>"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: application/x-stuff;"
    ; "  name*0*=us-ascii''This%20is%20even%20more%20;"
    ; "  name*1*=%2A%2A%2Afun%2A%2A%2A%20;"
    ; "  name*2=\"isn't it!\";"
    ; "Content-Transfer-Encoding: base64"
    ; "Content-Disposition: attachment;"
    ; "  filename*0*=us-ascii''This%20is%20even%20more%20;"
    ; "  filename*1*=%2A%2A%2Afun%2A%2A%2A%20;"
    ; "  filename*2=\"isn't it!\";"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: application/x-stuff;"
    ; "  name*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A"
    ; "Content-Transfer-Encoding: base64"
    ; "Content-Disposition: attachment;"
    ; "  filename*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: application/x-stuff;"
    ; "  name*0=very;name*1=-long-;name*2=name;"
    ; "Content-Disposition: attachment;"
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: application/x-stuff;"
    ; "  name*1=-backwards;name*0=very;name*2=-name"
    ; "Content-Disposition: attachment;"
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: application/x-stuff;"
    ; "  name*=us-ascii''file-with-charset;"
    ; "  charset=utf-8"
    ; "Content-Disposition: attachment;"
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "--BOUNDARY1"
    ; "Content-Type: application/x-stuff;"
    ; "  name*0=T;"
    ; "  name*1=o;"
    ; "  name*2=o;"
    ; "  name*3=\" \";"
    ; "  name*4=m;"
    ; "  name*5=a;"
    ; "  name*6=n;"
    ; "  name*7=y;"
    ; "  name*8=\" \";"
    ; "  name*9=p;"
    ; "  name*10=a;"
    ; "  name*11=r;"
    ; "  name*12=t;"
    ; "  name*13=s;"
    ; "  name*14=!;"
    ; "  charset=utf-8"
    ; "Content-Disposition: attachment;"
    ; "Content-Transfer-Encoding: base64"
    ; ""
    ; "--BOUNDARY1--"
    ]
  in
  parse_attachments'
    ~replace_attachment:(fun ~name:_ -> true)
    rfc2231_email_with_split_attachment_name;
  [%expect
    {|
    ((attachments
      ((((filename "This%20is%20even%20more%20%2A%2A%2Afun%2A%2A%2A%20isn't it!")
         (path (1)))
        "")
       (((filename This%20is%20%2A%2A%2Afun%2A%2A%2A) (path (2))) "")
       (((filename very-long-name) (path (3))) "")
       (((filename very-backwards-name) (path (4))) "")
       (((filename file-with-charset) (path (5))) "")
       (((filename "Too many parts!") (path (6))) "")))
     (stripped
      ((headers ((Content-Type " multipart/mixed; boundary=BOUNDARY1")))
       (raw_content
        ( "--BOUNDARY1\
         \nContent-Type: text/html; charset=utf-8\
         \nContent-Transfer-Encoding: quoted-printable\
         \n\
         \n<div>Simple Body</div>\
         \n\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1\
         \nContent-Transfer-Encoding: quoted-printable\
         \nContent-Type: text/plain; charset=\"UTF-8\"\
         \n\
         \n<REPLACED>\
         \n--BOUNDARY1--")))))
    |}];
  return ()
;;
