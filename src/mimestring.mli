open Core.Std

(* For usage in functors *)
module type S = sig
  type t = private string with sexp, bin_io;;
  val hash : t -> int
  val of_string : string -> t
  val to_lowercase_string : t -> string
  val equal_string : t -> string -> bool

  include Comparable.S with type t := t
end

module Case_insensitive : S with type t = string

val quote : string -> string
