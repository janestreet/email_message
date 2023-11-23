# Description

This library enables you to extract the content of `.eml` files.

## Example

Here's a basic example to get started:

```ocaml
(* ./bin/main.ml *)

module Email = Email_message.Email

let read_all path = In_channel.with_open_bin path In_channel.input_all

type email =
  { subject : string option
  ; from : string option
  ; attachments : string list
  }

let make_email path =
  try
    let content = read_all path in
    (* NOTE: [Email.of_string] mail fail *)
    let email = Email.of_string content in
    let subject = email |> Email.Simple.subject in
    let attachments =
      email |> Email.Simple.all_attachments |> List.map Email.Simple.Attachment.filename
    in
    let from = email |> Email.Simple.from |> Option.map Email_address.to_string in
    Ok { subject; from; attachments }
  with
  | e -> Error e
;;

let pretty_str lst =
  let items = lst |> List.map (Printf.sprintf {|"%s"|}) |> String.concat "; " in
  "[" ^ items ^ "]"
;;

let () =
  match make_email "/tmp/tmp.eml" with
  | Error exn ->
    print_endline "Sorry, I could not handle reading this email! Error was:";
    print_endline @@ Printexc.to_string exn;
    ()
  | Ok e ->
    print_endline "Printing the email content...";
    print_endline "---";
    print_endline @@ "Subject: " ^ Option.value ~default:"NONE" e.subject;
    print_endline @@ "From: " ^ Option.value ~default:"NONE" e.from;
    print_endline @@ "Attachments: " ^ pretty_str e.attachments;
    ()
;;
```
