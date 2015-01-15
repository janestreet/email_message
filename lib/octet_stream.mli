open Core.Std

(* RFC 2045 MIME-encoded Bigstrings. *)

module Encoding : sig
  (** Text or binary are the type of the plaintext. For Base64, if the mode is
      text, '\n' is turned into '\r\n' when encoding, and vice versa. *)
  type known =
    [ `Base64 of [ `Text | `Binary ]
    | `Bit7
    | `Bit8
    | `Binary
    | `Quoted_printable of [ `Text | `Binary ]
    ]
  with sexp, bin_io, compare

  type t =
    [ known
    | `Unknown of string
    ] with sexp, bin_io, compare
  ;;

  (* RFC 2045 says 7bit should be assumed if the Content-Transfer-Encoding heading is
     missing. *)
  val default : known

  val of_headers_or_default : Headers.t -> t
end

type t with sexp, bin_io, compare

include String_monoidable.S with type t := t
include Stringable.S with type t := t

val hash : t -> int

(*
(** mode defaults to `Text *)
include Bigstringable.S with type t := t
include Lexable.S with type t := t
*)

val empty : t

val create : ?encoding:Encoding.t -> Bigstring_shared.t -> t
val encoding : t -> Encoding.t
val encoded_contents : t -> Bigstring_shared.t

(* These are the expensive operation. *)
val encode : Bigstring_shared.t -> Encoding.known -> t
(* None if encoding is `Unknown. *)
val decode : t -> Bigstring_shared.t option

(*
val empty : t
val length : t -> int
val sub : ?pos:int -> ?len:int -> t -> t
*)
