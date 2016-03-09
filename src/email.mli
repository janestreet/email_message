open! Core.Std

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
  Headers.Content_type and Headers.Content_transfer_encoding, and the corresponding default
  values.
*)
val of_bigbuffer : Bigbuffer.t -> t Or_error.t

type email = t

module Multipart : sig
  type t =
    { boundary : Boundary.t
    ; prologue : Bigstring_shared.t option
    ; epilogue : Bigstring_shared.t option
    ; parts    : email list
    }
end

module Content : sig
  type t =
    | Multipart of Multipart.t
    | Data of Octet_stream.t
    | Message of email
end

val headers     : t -> Headers.t

val last_header : ?whitespace:Headers.Whitespace.t -> t -> Headers.Name.t -> Headers.Value.t option
val find_all_headers : ?whitespace:Headers.Whitespace.t -> t -> Headers.Name.t -> Headers.Value.t list

val set_headers : t -> Headers.t -> t

val modify_headers : t -> f:(Headers.t -> Headers.t) -> t

val add_header : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t
val add_headers : ?whitespace:Headers.Whitespace.t -> t -> (Headers.Name.t * Headers.Value.t) list -> t
val set_header : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t

val add_header_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t
val add_headers_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> (Headers.Name.t * Headers.Value.t) list -> t
val set_header_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> name:Headers.Name.t -> value:Headers.Value.t -> t

val filter_headers : ?whitespace:Headers.Whitespace.t -> t -> f:(name:Headers.Name.t -> value:Headers.Value.t -> bool) -> t
val map_headers : ?whitespace:Headers.Whitespace.t -> t -> f:(name:Headers.Name.t -> value:Headers.Value.t -> Headers.Value.t) -> t

val content     : t -> Content.t
val set_content : t -> Content.t -> t

(** Always sets [line_break = true] *)
val create
  :  headers : Headers.t
  -> content : Content.t
  -> t

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
  module Mimetype : sig
    type t = string
    val text : t
    val html : t
    val pdf : t
    val jpg : t
    val png : t

    val multipart_mixed : t
    val multipart_related : t
    val multipart_alternative : t

    val from_filename : string -> t
    val from_extension : string -> t

    val guess_encoding : t -> Octet_stream.Encoding.known
  end

  type attachment_name = string

  module Content : sig
    type t = private email
    val of_email : email -> t

    val create
      :  content_type:Mimetype.t
      -> ?encoding:(Octet_stream.Encoding.known)
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> string
      -> t

    val html : ?extra_headers:(Headers.Name.t * Headers.Value.t) list -> string -> t
    val text : ?extra_headers:(Headers.Name.t * Headers.Value.t) list -> string -> t

    val of_file
      :  ?content_type:Mimetype.t
      -> ?encoding:(Octet_stream.Encoding.known)
      -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> string
      -> t Async.Std.Deferred.t

    (* Combine 2 or more contents as alternative versions.
       List should be sorted from worst to best. *)
    val alternatives
      :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> t list
      -> t

    (* Add related resources (e.g. inline images).
       reference them using 'cid:${attachment_name}' in the content.
       To attach files you should use [create ~attachments] *)
    val with_related
      :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> resources:(attachment_name * t) list
      -> t
      -> t

  end

  type t = email

  val create
    :  ?from:Email_address.t (* defaults to <user@host> *)
    -> to_:Email_address.t list
    -> ?cc:Email_address.t list
    -> subject:string
    -> ?id:string
    -> ?date:Time.t
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?attachments:(attachment_name * Content.t) list
    -> Content.t
    -> t

  module Expert : sig
    val content
      :  whitespace:Headers.Whitespace.t
      -> extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> encoding:Octet_stream.Encoding.known
      -> string
      -> t
    val multipart
      :  whitespace:Headers.Whitespace.t
      -> content_type:Mimetype.t
      -> extra_headers:(Headers.Name.t * Headers.Value.t) list
      -> t list
      -> t
  end
end
