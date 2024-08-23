open Core
open Angstrom

module Charset = struct
  (* The following might not be an exhaustive list. We can add to this as we encounter
       more cases. *)
  type t =
    [ `Ascii
    | `Big5
    | `GB2312
    | `Latin1
    | `Latin2
    | `Utf8
    | `Windows1252
    ]
  [@@deriving sexp_of, enumerate, compare, equal]

  let to_string = function
    | `Ascii -> "US-ASCII"
    | `Big5 -> "BIG5"
    | `GB2312 -> "GB2312"
    | `Latin1 -> "ISO-8859-1"
    | `Latin2 -> "ISO-8859-2"
    | `Utf8 -> "UTF-8"
    | `Windows1252 -> "WINDOWS-1252"
  ;;
end

module Let_syntax = struct
  let bind t ~f = t >>= f
  let map t ~f = t >>| f
  let both a b = lift2 Tuple2.create a b
end

let ws = take_while1 Char.is_whitespace

let charset charsets =
  List.map charsets ~f:(fun charset ->
    string_ci (Charset.to_string charset) >>| const charset)
  |> choice
;;

let encoding : [ `Base64 | `Quoted_printable ] Angstrom.t =
  choice [ string_ci "B" >>| const `Base64; string_ci "Q" >>| const `Quoted_printable ]
;;

let parser_ : Charset.t list -> (Charset.t * string) Angstrom.t =
  fun charsets ->
  let%bind () = string "=?" >>| ignore
  and charset = charset charsets
  and () = string "?" >>| ignore
  and encoding
  and () = string "?" >>| ignore
  and data =
    take_while (function
      | '?' -> false
      | c -> (not (Char.is_whitespace c)) && Char.is_print c)
  and () = string "?=" >>| ignore in
  let%bind data =
    match encoding with
    | `Quoted_printable ->
      (* RFC2047 deviates slightly from common quoted printable.
         In particular
         4.2(2) - Underscore may be used to encode space, and
         4.2(3)- underscore must be encoded.
         This substituion handles that decoding step. *)
      let data = String.substr_replace_all data ~pattern:"_" ~with_:" " in
      let data_bstr, _ =
        Quoted_printable_lexer.decode_quoted_printable
          (String.length data)
          (Lexing.from_string data)
      in
      return (Bigbuffer.contents data_bstr)
    | `Base64 ->
      (match Base64.decode data with
       | Ok data -> return data
       | Error (`Msg msg) -> fail msg)
  in
  match charset with
  | `Ascii | `Utf8 | `Latin1 | `Latin2 | `GB2312 | `Windows1252 | `Big5 ->
    return (charset, data)
;;

let parser_many
  :  (Charset.t as 'a) list
  -> [ `Encoded of 'a * string | `Plain of string ] list Angstrom.t
  =
  fun charsets ->
  many
    (choice
       [ (let%map hd = parser_ charsets
          and tl =
            (* RFC2047 6.2 When displaying a particular header field that contains
               multiple 'encoded-word's, any 'linear-white-space' that separates a
               pair of adjacent 'encoded-word's is ignored. *)
            many
              (let%map (_ : string) = option "" ws
               and res = parser_ charsets in
               `Encoded res)
          in
          `Encoded hd :: tl)
       ; (let%map c =
            choice
              [ take_while1 (function
                  | '=' -> false
                  | c -> not (Char.is_whitespace c))
              ; string "="
                (* Collapse Line breaks as per
                 RFC822 - 3.1.1 Unfolding is accomplished by regarding CRLF immediately
                 followed by an LWSP-char as equivalent to the LWSP-char.
                 RFC822 - 3.1.3 Rules of (un)folding apply to these (unstructured) fields *)
              ; (let%bind (_ : string) = choice [ string "\r\n"; string "\n" ] in
                 ws)
                (* The RFC is ambiguous on what should happen if there is a lone CRLF, so we
                 ignore those, and treat these as regular white space. The RFC is also
                 ambiguous on how to treat multiple consecutive whitespaces, so we do the
                 conservative thing and leave them exactly as is. *)
              ; ws
              ]
          in
          [ `Plain c ])
       ])
  >>| List.concat
;;

let decode_with_charset ?(charsets = Charset.all) str =
  Angstrom.parse_string ~consume:Prefix (parser_many charsets) str
  |> Result.map_error ~f:Error.of_string
;;

let decode ?(charsets = Charset.all) str =
  let%map.Or_error chunks = decode_with_charset ~charsets str in
  List.map chunks ~f:(function
    | `Plain str -> str
    | `Encoded (_charset, str) -> str)
  |> String.concat ~sep:""
;;
