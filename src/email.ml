open! Core
open! Async
open! Import
include Email_message_kernel

let save ?temp_file ?perm ?fsync ?eol_except_raw_content t path =
  Writer.with_file_atomic ?temp_file ?perm ?fsync path ~f:(fun writer ->
    String_monoid.iter (to_string_monoid ?eol_except_raw_content t) ~f:(function
      | String_monoid.Underlying.Char c -> Writer.write_char writer c
      | String str -> Writer.write writer str
      | Bigstring bstr -> Writer.schedule_bigstring writer bstr);
    return ())
;;

module Content = Email_message_kernel.Content
module Raw_content = Email_message_kernel.Raw_content
module Simple = Email_simple
