open! Core

module Charset : sig
  type t =
    [ `Ascii
    | `Big5
    | `GB2312
    | `Latin1
    | `Latin2
    | `Utf8
    | `Windows1252
    ]
  [@@deriving sexp_of]

  val all : t list
end

(** Like [decode_with_charset], but completely ignores all charset information and simply
    concats all of the bytes together. *)
val decode : ?charsets:Charset.t list -> string -> string Or_error.t

(** Decodes words encoded as per: https://tools.ietf.org/html/rfc2047 *)
val decode_with_charset
  :  ?charsets:Charset.t list
  -> string
  -> [ `Plain of string | `Encoded of Charset.t * string ] list Or_error.t

(** Like [decode_with_charset], but supports arbitrary charsets, not just the ones in
    [Charset.t].


*)
val decode_with_raw_charset
  :  string
  -> [ `Plain of string | `Encoded of [ `Charset of string ] * string ] list Or_error.t
