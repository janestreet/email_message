open Core.Std

module type S = sig
  type t = string with sexp;;
  val compare : t -> t -> int;;
  val equal : t -> t -> bool;;
  val hash : t -> int;;
end

(** Case-insensitive strings *)
module Case_insensitive = struct
  type t = string with sexp

  let compare x y = String.compare (String.lowercase x) (String.lowercase y)
  let equal x y = String.equal (String.lowercase x) (String.lowercase y)
  let hash x = String.hash (String.lowercase x)
end

let quote_escape =
  unstage (String.Escaping.escape ~escapeworthy:['"'; '\\'] ~escape_char:'\\')
;;

let quote str = String.concat ["\""; quote_escape str; "\""];;




