open! Core

(* RFC 2045 MIME-encoded Bigstrings. *)
module type Encoding = sig
  (** Text or binary are the type of the plaintext. For Base64, if the mode is
      text, '\n' is turned into '\r\n' when encoding, and vice versa. *)
  type known =
    [ `Base64
    | `Bit7
    | `Bit8
    | `Binary
    | `Quoted_printable
    ]
  [@@deriving sexp_of, compare, hash]

  type t =
    [ known
    | `Unknown of string
    ]
  [@@deriving sexp_of, compare, hash]

  (* RFC 2045 says 7bit should be assumed if the Content-Transfer-Encoding heading is
     missing. *)

  val default : known
  val default' : t

  include Stringable.S with type t := t
end

module type S = sig
  module Encoding : Encoding

  type t [@@deriving sexp_of, compare, hash]

  val of_string : encoding:Encoding.t -> string -> t
  val of_bigstring_shared : encoding:Encoding.t -> Bigstring_shared.t -> t
  val empty : t
  val encoding : t -> Encoding.t
  val encoded_contents : t -> Bigstring_shared.t
  val encoded_contents_string : t -> string

  (** These are the expensive operation. *)

  val encode : encoding:Encoding.known -> Bigstring_shared.t -> t

  (** Return [None] if encoding is `Unknown. *)
  val decode : t -> Bigstring_shared.t option
end

module type Octet_stream = sig
  module type Encoding = Encoding
  module type S = S
end
