open! Core

module type Mimetype = sig
  type t = private string [@@deriving compare, sexp_of]

  val text : t
  val text_utf8 : t
  val html : t
  val html_utf8 : t
  val pdf : t
  val jpg : t
  val png : t
  val csv : t
  val multipart_mixed : t
  val multipart_related : t
  val multipart_alternative : t
  val of_string : string -> t
  val equal : t -> t -> bool
  val arg_type : t Command.Arg_type.t
  val from_filename : string -> t
  val from_extension : string -> t
  val to_extension : t -> string option
  val guess_encoding : t -> Octet_stream.Encoding.known
end

module type Content = sig
  type attachment_name

  module Mimetype : T

  type t = private Email.t [@@deriving sexp_of]

  val of_email : Email.t -> t

  val create_custom
    :  content_type:Mimetype.t
    -> ?encoding:Octet_stream.Encoding.known
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t

  val create
    :  content_type:Mimetype.t
    -> ?encoding:Octet_stream.Encoding.known
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t
    [@@deprecated "[since 2019-08] Renamed to [create_custom]"]

  val html_utf8
    :  ?encoding:Octet_stream.Encoding.known (** default: `Quoted_printable *)
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t

  val html
    :  ?encoding:Octet_stream.Encoding.known (** default: `Quoted_printable *)
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t
    [@@deprecated "[since 2019-08] Please specify the charset, e.g. [html_utf8]"]

  val text_utf8
    :  ?encoding:Octet_stream.Encoding.known (** default: `Quoted_printable *)
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t

  val text
    :  ?encoding:Octet_stream.Encoding.known (** default: `Quoted_printable *)
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t
    [@@deprecated "[since 2019-08] Please specify the charset, e.g. [text_utf8]"]

  (** Plain text e-mail that also includes an html version so it's displayed
      monospace in gmail.

      By default, we add some custom styling to disable the line-wrap formatting rule
      which gmail uses. To disable this behavior, supply [~force_no_line_wrap:false].
  *)
  val text_monospace_utf8
    :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?force_no_line_wrap:bool (** default: true *)
    -> string
    -> t

  val text_monospace
    :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?force_no_line_wrap:bool (** default: true *)
    -> string
    -> t
    [@@deprecated
      "[since 2019-08] Please specify the charset, e.g. [text_monospace_utf8]"]

  (** Combine 2 or more contents as alternative versions.
      List should be sorted from worst to best. *)
  val alternatives : ?extra_headers:(Headers.Name.t * Headers.Value.t) list -> t list -> t

  (** Combine 2 or more contents that should be bundled together *)
  val mixed : ?extra_headers:(Headers.Name.t * Headers.Value.t) list -> t list -> t

  (** Add related resources (e.g. inline images).
      You can reference them using 'cid:${attachment_name}' in the content.
      To attach files you should use [create ~attachments] *)
  val with_related
    :  ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> resources:(attachment_name * t) list
    -> t
    -> t

  val content_type : t -> Mimetype.t

  (** The Content-ID of the content *)
  val related_part_cid : t -> attachment_name option

  val all_related_parts : t -> (attachment_name * t) list
  val find_related : t -> attachment_name -> t option

  (** [content] and [parts] return [None] if the email doesn't properly parse. They also
      return [None] if the message has content type "message/rfc822" *)
  val content : t -> Octet_stream.t option

  val parts : t -> t list option

  (** Get the alternative versions available. If the message is not of content type
      "multipart/alternative" then return a singleton list. *)
  val alternative_parts : t -> t list

  (** Get the 'inline' parts, This expands "Content-Type: multipart/{mixed,related}",
      stripping out any attachment parts. multipart/alternative is not expanded *)
  val inline_parts : t -> t list

  val content_disposition : t -> [ `Inline | `Attachment of string ]
  val attachment_name : t -> string option
end

module type Expert = sig
  type t
  type attachment_name

  module Mimetype : T
  module Content : T

  val create_raw
    :  from:string (** defaults to <user@host> *)
    -> to_:string list
    -> ?cc:string list
    -> ?reply_to:string
    -> subject:string
    -> id:string
    -> ?in_reply_to:string
    -> date:string
    -> ?auto_generated:unit
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?attachments:(attachment_name * Content.t) list
    -> Content.t
    -> t

  val content
    :  normalize_headers:Headers.Normalize.encode
    -> extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> encoding:Octet_stream.Encoding.known
    -> string
    -> t

  val multipart
    :  normalize_headers:Headers.Normalize.encode
    -> content_type:Mimetype.t
    -> extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> t list
    -> t
end

module type Stable = sig
  module Content : sig
    type latest

    module V1 : sig
      type t = latest [@@deriving bin_io, sexp]
    end
  end

  module Mimetype : sig
    type latest

    module V1 : Stable_without_comparator with type t = latest
  end
end

module type Email_simple = sig
  module Mimetype : Mimetype

  type attachment_name = string [@@deriving sexp_of]

  module Content :
    Content with module Mimetype := Mimetype and type attachment_name := attachment_name

  type t = Email.t [@@deriving sexp_of]

  val create
    :  from:Email_address.t (** defaults to <user@host> *)
    -> to_:Email_address.t list
    -> ?cc:Email_address.t list
    -> ?reply_to:Email_address.t
    -> subject:string
    -> id:string
    -> ?in_reply_to:string
    -> date_string:string
    -> ?auto_generated:unit
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?attachments:(attachment_name * Content.t) list
    -> Content.t
    -> t

  val from : t -> Email_address.t option
  val to_ : t -> Email_address.t list option
  val cc : t -> Email_address.t list option
  val subject : t -> string option
  val id : t -> string option

  (** [extract_body ?content_type t] returns the body associated with the email part that
      matches the [content_type] mimetype, or none if [t] does not contain a body or part of
      type [content_type]. *)
  val extract_body
    :  ?content_type:Mimetype.t (** default: [Mimetype.text] *)
    -> t
    -> string option

  (** Related parts are those that are included in a multi-part message with a "Content-ID"
      header. This content can be referenced by adding the "cid:" prefix and stripping the
      enclosing '<' and '>'.

      For example (from https://tools.ietf.org/html/rfc2392):

      {v
        From: foo1@bar.net
        To: foo2@bar.net
        Subject: A simple example
        Mime-Version: 1.0
        Content-Type: multipart/related; boundary="boundary-example-1"; type=Text/HTML
        --boundary-example 1
        Content-Type: Text/HTML; charset=US-ASCII

        to the other body part, for example through a statement such as:
        <IMG SRC="cid:foo4*foo1@bar.net" ALT="IETF logo">

        --boundary-example-1

        Content-ID: <foo4*foo1@bar.net>
        Content-Type: IMAGE/GIF
        Content-Transfer-Encoding: BASE64

        R0lGODlhGAGgAPEAAP/////ZRaCgoAAAACH+PUNvcHlyaWdodCAoQykgMTk5
        NSBJRVRGLiBVbmF1dGhvcml6ZWQgZHVwbGljYXRpb24gcHJvaGliaXRlZC4A
        etc...

        --boundary-example-1--
      v}

      Calling [all_related_parts] on this email would return a list of length one where the
      [attachment_name] is "foo4*foo1@bar.net" for the single entry.

      Related parts are often used for inline images. *)
  val all_related_parts : t -> (attachment_name * Content.t) list

  val find_related : t -> attachment_name -> Content.t option
  val inline_parts : t -> Content.t list

  module Expert :
    Expert
      with module Mimetype := Mimetype
       and module Content := Content
       and type t := t
       and type attachment_name := attachment_name

  module Stable :
    Stable with type Content.latest := Content.t and type Mimetype.latest := Mimetype.t
end
