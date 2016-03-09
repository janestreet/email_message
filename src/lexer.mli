open! Core.Std

val message : Lexer_state.t -> Lexing.lexbuf -> Grammar.token
val content_type : Lexing.lexbuf -> Grammar.token
val find_boundary : string -> Lexing.lexbuf ->
  [ `Open_boundary_first of int |
    `Open_boundary of (int * int) |
    `Close_boundary of (int * int) |
    `Eof ]

val is_rfc2045_token : Lexing.lexbuf -> bool

(*
val field_unstructured_fold : Bigbuffer.t -> Lexing.lexbuf -> unit
val field_unstructured_unfold : Bigbuffer.t -> Lexing.lexbuf -> unit
*)

(* base64 is always encoded as binary. *)
val decode_base64 : int -> Lexing.lexbuf ->
  (Bigbuffer.t * [`Ok | `Unexpected_characters | `Wrong_padding ])
;;

val encode_base64 : int -> Lexing.lexbuf -> Bigbuffer.t

val decode_quoted_printable : int -> Lexing.lexbuf ->
  (Bigbuffer.t * [`Ok | `Unexpected_characters ])
;;

(* quoted printable is ALWAYS encoded as text *)
val encode_quoted_printable : int -> Lexing.lexbuf -> Bigbuffer.t
