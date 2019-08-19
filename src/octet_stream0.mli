open! Core
module Encoding0 : Octet_stream_intf.Encoding
include Octet_stream_intf.S with module Encoding := Encoding0

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end
