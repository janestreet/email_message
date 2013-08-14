open Core.Std

type t = Bigstring.t

(** Empty, immutable Bigstring *)
val empty : t

(** Creates a lexing buffer to be used with the lexing module. *)
val to_lexbuf : t -> Lexing.lexbuf

val foldi : t -> init:'b -> f:(int -> 'b -> char -> 'b) -> 'b

(** Gets a bigstring from a bigbuffer with minimal memory overhead. *)
val of_bigbuffer_volatile : Bigbuffer.t -> Bigstring.t



