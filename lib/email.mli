open Core.Std

type t

val empty : unit -> t

(** The cost depends on the encoding of the content and the main media type.

  N = Size of the message
  H = Size of the headers of the sub-message(s)

  Format: time complexity, memory complexity

            | 7bit, 8bit, binary | Base64, Quoted_printable
  -------------------------------------------------------------
  message   |    O(N), O(H)      | O(N), O(N)
  multipart |    O(N), O(H)      | O(N), O(N)
  *         |    O(1), O(1)      | O(N), O(N)

  Where * is any other main media type: text, image, application...

  Encoding and type can be obtained from the headers, using the modules
  Header.Content_type and Header.Content_transfer_encoding, and the corresponding default
  values.
*)
val of_bigbuffer : Bigbuffer.t -> t Or_error.t

val headers     : t -> Headers.t
val set_headers : t -> Headers.t -> t

val content
  :  t
  -> [ `Message of t
     | `Data of Octet_stream.t
     | `Multipart of (t list)]

(** Allow changing the message content to mask the actual data but retain the
    structure *)
val map_data : t -> f:(Octet_stream.t -> Octet_stream.t) -> t

(** The content of the body itself, without headers. *)
val raw_content : t -> Bigstring_shared.t

val to_bigstring_shared : t -> Bigstring_shared.t

(** String-builder-like module. Small-to-no memory overhead
    when unparsed. *)
include String_monoidable.S with type t := t

include Stringable.S    with type t := t
include Bigstringable.S with type t := t
include Sexpable.S      with type t := t
include Comparable.S    with type t := t

include Binable.S       with type t := t

val hash : t -> int

module Simple : sig
  val create     : headers:(string * string) list -> body:string -> t Or_error.t
  val create_exn : headers:(string * string) list -> body:string -> t
end
