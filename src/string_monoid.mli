open! Core
open! Async

include Email_message_kernel.Private.String_monoid_intf.String_monoid (** @inline *)

(*_
  For the library to fulfill it's purpose of minimal overhead
  string concatenation, the output functions must be tightly
  coupled with the low-level representation.

  Any new output channel should be implemented as new methods
  of the library itself.
*)

val output_unix : t -> Writer.t -> unit
val output_channel : t -> Out_channel.t -> unit
