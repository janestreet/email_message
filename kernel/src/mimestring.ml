open Core.Core_stable

(** Case-insensitive strings *)
module Case_insensitive = struct
  module Stable = struct
    module V1 = struct
      type t = String.V1.t [@@deriving sexp, bin_io]

      let compare = Core.String.Caseless.compare
      let comparator = Core.String.Caseless.comparator

      type comparator_witness = Core.String.Caseless.comparator_witness

      let hash = Core.String.Caseless.hash
      let hash_fold_t = Core.String.Caseless.hash_fold_t
    end
  end

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
