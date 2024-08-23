(** Simple library for concatenating immutable strings efficiently *)

open! Core

module type Underlying = sig
  type t =
    | String of String.t
    | Bigstring of Bigstring.t
    | Char of char
end

module type String_monoid = sig
  type t

  (** Primitive, constant-time operations *)
  val empty : t

  val nl : t
  val plus : t -> t -> t
  val length : t -> int

  (** Linear in the number of elements. *)
  val concat : ?sep:t -> t list -> t

  (** Linear in the number of elements in the list. *)
  val concat_string : ?sep:string -> string list -> t

  (*_ t_of_* is O(1), *_of_t is O(N), N being the length *)

  include Stringable.S with type t := t

  val of_bigstring : Bigstring.t -> t
  val to_bigstring : t -> Bigstring.t
  val of_char : char -> t

  (*_
    For the library to fulfill it's purpose of minimal overhead
    string concatenation, the output functions must be tightly
    coupled with the low-level representation.

    Any new output channel should be implemented as new methods
    of the library itself.
  *)

  val output_bigbuffer : t -> Bigbuffer.t -> unit

  module Underlying : Underlying

  val fold : t -> init:'accum -> f:local_ ('accum -> Underlying.t -> 'accum) -> 'accum
  val iter : t -> f:(Underlying.t -> unit) -> unit
  val is_suffix : t -> suffix:string -> bool
  val is_prefix : t -> prefix:string -> bool
  val is_substring : t -> substring:string -> bool
  val is_string : t -> string:string -> bool
end
