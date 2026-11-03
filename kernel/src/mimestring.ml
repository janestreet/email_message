(** Case-insensitive strings *)
module Case_insensitive = struct
  open Core
  include String.Caseless

  let of_string = Fn.id
  let to_string = Fn.id
  let to_lowercase_string = String.lowercase
  let equal_string = equal
end

open Core

module type S = sig
  type t [@@deriving sexp]

  val of_string : string -> t
  val to_lowercase_string : t -> string
  val equal_string : t -> string -> bool

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

let quote_escape =
  unstage (String.Escaping.escape ~escapeworthy:[ '"'; '\\' ] ~escape_char:'\\')
;;

let quote str = String.concat [ "\""; quote_escape str; "\"" ]
