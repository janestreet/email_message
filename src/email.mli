open! Core
open! Async
open! Import

include Email_message_kernel.Private.Email_intf.Email (** @inline *)

(** Efficiently save [t] to disk with little additional allocation.

    [?temp_file], [?perm], [?fsync] are blindly passed to [Writer.with_file_atomic] *)
val save
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> ?eol_except_raw_content:Lf_or_crlf.t (** default is [`LF] *)
  -> t
  -> string
  -> unit Deferred.t

module Content = Email_message_kernel.Content
module Raw_content = Email_message_kernel.Raw_content
module Simple = Email_simple
