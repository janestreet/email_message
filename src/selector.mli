open! Core
open! Import

module Base : sig
  (*_ The "exists" query can equally be accomplished using [sexp query], but
    not the "all" one. *)

  type t =
    [ `exists_header of string * Re2.t
    | `all_headers of string * Re2.t
    ]
  [@@deriving sexp_of]

  val matches : t -> Email.t -> bool
  val matches' : t -> Headers.t -> bool
  val examples : t list
end

type t = Base.t Blang.t [@@deriving sexp_of]

val matches : t -> Email.t -> bool
val matches' : t -> Headers.t -> bool
val example : t

module Stable : sig
  module Base : sig
    module V1 : sig
      type t = [ | Base.t ] [@@deriving bin_shape, sexp]
    end
  end

  module V1 : sig
    type nonrec t = t [@@deriving bin_shape, sexp]
  end
end
