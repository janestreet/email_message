open Core.Std

(** Immutable sequences of bytes which can be windowed efficiently. *)
type t = private Bigstring.t with sexp, bin_io, compare

val of_bigstring : Bigstring.t -> t
val to_bigstring : t -> Bigstring.t

include Stringable.S with type t := t
include String_monoidable.S with type t := t
include Lexable.S with type t := t

val hash : t -> int

(** Empty, immutable Bigstring *)
val empty : t

val length : t -> int
val sub : ?pos:int -> ?len:int -> t -> t

val foldi : t -> init:'b -> f:(int -> 'b -> char -> 'b) -> 'b

val split_lines : t -> t list

(** Gets a bigstring from a bigbuffer with minimal memory overhead. *)
val of_bigbuffer_volatile : Bigbuffer.t -> t

val of_string_monoid : String_monoid.t -> t
