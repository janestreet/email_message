open! Core

(* For usage in functors *)
module type S = sig
  type t = private string [@@deriving sexp_of]
  val of_string : string -> t
  val to_lowercase_string : t -> string
  val equal_string : t -> string -> bool

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module Case_insensitive : S with type t = string

val quote : string -> string
