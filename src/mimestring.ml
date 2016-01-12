open Core.Std

module type S = sig
  type t = private string [@@deriving sexp, bin_io];;
  val hash : t -> int
  val of_string : string -> t
  val to_lowercase_string : t -> string
  val equal_string : t -> string -> bool

  include Comparable.S with type t := t
end

(** Case-insensitive strings *)
module Case_insensitive = struct
  module T = struct
    type t = string [@@deriving sexp, bin_io]

    let compare x y = String.compare (String.lowercase x) (String.lowercase y)
    let hash x = String.hash (String.lowercase x)
    let of_string t = t
    let to_lowercase_string t = String.lowercase t
  end
  include T
  include Comparable.Make(T)

  let equal_string t s =
    equal t (of_string s)
end

let quote_escape =
  unstage (String.Escaping.escape ~escapeworthy:['"'; '\\'] ~escape_char:'\\')
;;

let quote str = String.concat ["\""; quote_escape str; "\""];;
