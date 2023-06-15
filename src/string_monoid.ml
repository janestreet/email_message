open! Core
open! Import
include Email_message_kernel.String_monoid

module Underlying = struct
  include Underlying

  let output_channel ~channel = function
    | String str -> Out_channel.output_string channel str
    | Bigstring bstr -> Bigstring_unix.really_output channel bstr
    | Char c -> Out_channel.output_char channel c
  ;;

  let output_unix ~writer = function
    | String str -> Async.Writer.write writer str
    | Bigstring bstr -> Async.Writer.write_bigstring writer bstr
    | Char c -> Async.Writer.write_char writer c
  ;;
end

let output_channel t channel =
  Private.output ~dst_output:(Underlying.output_channel ~channel) t
;;

let output_unix t writer = Private.output ~dst_output:(Underlying.output_unix ~writer) t
