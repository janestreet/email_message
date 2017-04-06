open! Core

(** An [Email.t] is a list of headers along with unparsed content. [Email_content.parse]
    can be used to work with the structured content of an email. *)

type t [@@deriving sexp, bin_io, compare, hash]

val create : headers:Headers.t -> raw_content:Bigstring_shared.t -> t

val headers : t -> Headers.t
val set_headers : t -> Headers.t -> t
val modify_headers : t -> f:(Headers.t -> Headers.t) -> t

val raw_content : t -> Bigstring_shared.t
val set_raw_content : t -> Bigstring_shared.t -> t
val modify_raw_content : t -> f:(Bigstring_shared.t -> Bigstring_shared.t) -> t

val to_bigstring_shared : t -> Bigstring_shared.t

(** String-builder-like module. Small-to-no memory overhead
    when unparsed. *)
include String_monoidable.S with type t := t

include Stringable.S with type t := t
val to_bigstring : t -> Bigstring.t
val of_bigstring : Bigstring.t -> t
val of_bigbuffer : Bigbuffer.t -> t

include Sexpable.S      with type t := t
include Comparable.S    with type t := t
include Binable.S       with type t := t
