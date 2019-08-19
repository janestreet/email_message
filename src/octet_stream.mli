open! Core

module Encoding : sig
  include Octet_stream_intf.Encoding

  (** Determine an encoding based on email headers. [ignore_base64_for_multipart] is
      useful because some clients can't read RFCs and incorrectly indicate a transfer
      encoding of base64 for multipart messages. *)
  val of_headers_or_default
    :  ?ignore_base64_for_multipart:bool (** default: true *)
    -> Headers.t
    -> t
end

include
  Octet_stream_intf.S with type t = Octet_stream0.t with module Encoding := Encoding

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end
