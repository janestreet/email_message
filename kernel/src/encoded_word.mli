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

(** Encoded words as per: https://tools.ietf.org/html/rfc2047 *)
val decode : ?charsets:Charset.t list -> string -> string Or_error.t

(** Encoded words as per: https://tools.ietf.org/html/rfc2047 *)
val decode_with_charset
  :  ?charsets:Charset.t list
  -> string
  -> [ `Plain of string | `Encoded of Charset.t * string ] list Or_error.t
