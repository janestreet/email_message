open Core.Std

type t = [
  `Base64 |
  `Bit7 |
  `Bit8 |
  `Binary |
  `Quoted_printable |
  `Unknown of Mimestring.Case_insensitive.t
] with sexp
;;

(* The default encoding must always decode with no errors *)
let default = `Binary;;

let table = [
  (`Base64, "base64");
  (`Bit7, "7bit");
  (`Bit8, "8bit");
  (`Binary, "binary");
  (`Quoted_printable, "quoted-printable");
]
;;

let table' = List.Assoc.inverse table;;

let to_string = function
  | `Unknown str -> str
  | known        -> List.Assoc.find_exn table known
;;

let of_string str =
  let token = Lexer.field_token (Lexing.from_string str) in
  let token = Result.ok_or_failwith token in
  let token = Option.value token ~default:"" in
  match List.Assoc.find table' ~equal:Mimestring.Case_insensitive.equal token with
  | Some known -> known
  | None       -> `Unknown token
;;

let mode = function
  | `Base64 | `Quoted_printable | `Bit7 | `Bit8 -> `Text
  | `Binary | `Unknown _ -> `Binary
;;

let encode t octet_stream = match t with
  | `Base64 -> Result.Ok (Octet_stream.Base64.encode octet_stream)
  | `Quoted_printable ->
      Result.Ok (Octet_stream.Quoted_printable.encode octet_stream)
  | `Bit7 -> Result.Ok octet_stream
  | `Bit8 -> Result.Ok octet_stream
  | `Binary -> Result.Ok octet_stream
  | `Unknown token -> Result.Error (`Unknown token)
;;

let decode t ?media_type octet_stream =
  let mode = Option.map media_type ~f:Media_type.mode in
  let mode = Option.value mode ~default:`Text in
  let open Result.Monad_infix in
  begin
  match t with
  | `Base64 -> Result.Ok (Octet_stream.Base64.decode)
  | `Quoted_printable -> Result.Ok (Octet_stream.Quoted_printable.decode)
  | `Bit7 -> Result.Ok (Octet_stream.Identity.decode)
  | `Bit8 -> Result.Ok (Octet_stream.Identity.decode)
  | `Binary -> Result.Ok (Octet_stream.Identity.decode)
  | `Unknown token -> Result.Error (`Unknown token)
  end
  >>| fun f -> f ~mode octet_stream
;;

let decode_default t ?media_type octet_stream =
  match decode t ?media_type octet_stream with
  | Result.Ok octet_stream -> octet_stream
  | Result.Error (`Unknown _) ->
    Option.value_exn (Result.ok (decode default ?media_type octet_stream))
;;

let encode_default t octet_stream =
  match encode t octet_stream with
  | Result.Ok octet_stream -> octet_stream
  | Result.Error (`Unknown _) ->
    Option.value_exn (Result.ok (encode default octet_stream))
;;


