open! Core
open! Async
open! Import

(*_ break dependency cycle *)

module Email := Email_message_kernel

(** @inline *)
include
  Email_message_kernel.Private.Email_simple_intf.Email_simple
  with module Content := Email_message_kernel.Simple.Content
   and module Expert := Email_message_kernel.Simple.Expert
   and module Stable := Email_message_kernel.Simple.Stable

type attachment_name = string [@@deriving sexp_of]

(** For parsing attachments. Use [create ~attachments] to add attachments. Convenience
    functions for email parts that have "Content-Disposition: attachment" *)
module Attachment : sig
  module Id : sig
    type t [@@deriving compare, sexp_of]
  end

  type t

  (** In a given email, each attachment has a unique [Id.t] that is determined by the
      email structure. *)
  val id : t -> Id.t

  (** The headers surrounding this attachment *)
  val headers : t -> Headers.t

  (** [Some email] if this is an attached message/rfc822 content *)
  val embedded_email : t -> Email.t option

  (** These are expensive operations *)
  val raw_data : t -> Bigstring_shared.t Or_error.t

  val md5 : t -> string Or_error.t
  val sha256 : t -> string Or_error.t
  val filename : t -> attachment_name

  (** [filename] decoded as per [Headers.Encoded_word.decode] *)
  val decoded_filename : t -> attachment_name

  (** This function is similar to [decode_filename], but it includes charset details.
      Right now, the email attachment downloader uses it to convert attachment names to
      UTF-8 using lib/iconv. In the future, we might want to add UTF-8 conversion directly
      into [Email_message] and swap this out with [decode_filename_with_utf_8_conversion].
      However, we can't do that at the moment because of a vulnerability in iconv *)
  type decode_error =
    [ `Multiple_distinct_charsets_in_attachment_name of
      [ `Encoded of Headers.Encoded_word.Charset.t * attachment_name
      | `Plain of attachment_name
      ]
        Nonempty_list.t
    | `Empty_attachment_name
    ]

  val decoded_filename_with_charset
    :  t
    -> (Headers.Encoded_word.Charset.t option * attachment_name, decode_error) result

  val to_file : t -> string -> unit Deferred.Or_error.t
end

module Content : sig
  include Email_message_kernel.Private.Email_simple_intf.Content

  val of_file
    :  ?content_type:Mimetype.t
    -> ?encoding:Octet_stream.Encoding.known
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> string
    -> t Deferred.t

  (** Save content to disk *)
  val to_file : t -> string -> unit Deferred.Or_error.t
end

val create
  :  ?from:Email_address.t (** defaults to <user@host> *)
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?reply_to:Email_address.t
  -> subject:string
  -> ?id:string
  -> ?in_reply_to:string
  -> ?date:Time_float.t
  -> ?auto_generated:unit
  -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
  -> ?attachments:(attachment_name * Content.t) list
  -> ?no_tracing_headers:[ `Because_not_using_standard_email_infra ]
  -> Content.t
  -> t

val create_utf8
  :  ?from:Email_address.t (** defaults to <user@host> *)
  -> to_:Email_address.t list
  -> ?cc:Email_address.t list
  -> ?reply_to:Email_address.t
  -> subject:string
  -> ?id:string
  -> ?in_reply_to:string
  -> ?date:Time_float.t
  -> ?auto_generated:unit
  -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
  -> ?attachments:(attachment_name * Content.t) list
  -> ?no_tracing_headers:[ `Because_not_using_standard_email_infra ]
  -> Content.t
  -> t

(** A unique value to be used in a Message-Id header *)
val make_id : unit -> Headers.Value.t

val local_address : unit -> Email_address.t

(** [all_attachments] looks recursively through the e-mail parts, looking for attachments.

    [~include_inline_parts] (default is `None) controls whether this function will attempt
    to interpret inline parts as attachments. [`Named_or_has_content_id] most aggressively
    classifies parts as attachments, including inline parts that are either named or have
    a Content-Id header. [`Named] will include inline parts that are named.

    If [~look_through_attached_mails:true] (the default), it will separately include both
    e-mail attachments as well as the attachments to those e-mails. Otherwise it will
    include e-mail attachments but not (separately) any of the attached e-mails'
    attachments.

    [decode_filename_using] defines which character sets should be used to decode
    attachment filenames. This option is mainly there to keep legacy behaviour, where
    KS-C-5601 encoded filenames aren't decoded by default. *)
val all_attachments
  :  ?decode_filename_using:Headers.Encoded_word.Charset.t list
  -> ?include_inline_parts:[ `None | `Named | `Named_or_has_content_id ]
  -> ?look_through_attached_mails:bool
  -> t
  -> Attachment.t list

val find_attachment : t -> attachment_name -> Attachment.t option

(** [map_attachments] recurses into message/rfc822 parts. However, if a message/rfc822
    part is replaced, there is no further recursion.

    [decode_filename_using] defines which character sets should be used to decode
    attachment filenames. This option is mainly there to keep legacy behaviour, where
    KS-C-5601 encoded filenames aren't decoded by default. *)
val map_attachments
  :  ?include_inline_parts:[ `None | `Named | `Named_or_has_content_id ]
  -> ?decode_filename_using:Headers.Encoded_word.Charset.t list
  -> t
  -> f:(Attachment.t -> [ `Keep | `Replace of t ])
  -> t

module Expert : sig
  include Email_message_kernel.Private.Email_simple_intf.Expert

  val create_raw
    :  ?from:string (** defaults to <user> *)
    -> to_:string list
    -> ?cc:string list
    -> ?reply_to:string
    -> subject:string
    -> ?id:string
    -> ?in_reply_to:string
    -> ?date:string
    -> ?auto_generated:unit
    -> ?extra_headers:(Headers.Name.t * Headers.Value.t) list
    -> ?attachments:(attachment_name * Content.t) list
    -> ?no_tracing_headers:[ `Because_not_using_standard_email_infra ]
    -> Content.t
    -> t
end

module Stable : sig
  include Email_message_kernel.Private.Email_simple_intf.Stable

  module Attachment : sig
    module Id : sig
      module V1 : Stable_without_comparator with type t = Attachment.Id.t
    end
  end
end
