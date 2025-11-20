open Core

module Domain : sig
  type t = String.Caseless.t [@@deriving sexp_of, compare ~localize]

  include Stringable with type t := t

  include
    Comparable.S_plain
    with type t := t
     and type comparator_witness = String.Caseless.comparator_witness

  include Hashable.S_plain with type t := t
end

type t [@@deriving sexp_of, sexp_grammar, compare ~localize, hash]

val create : ?prefix:string -> ?domain:Domain.t -> string -> t
val of_string : ?default_domain:string -> string -> t Or_error.t
val of_string_exn : ?default_domain:string -> string -> t
val list_of_string : ?default_domain:string -> string -> t list Or_error.t
val list_of_string_exn : ?default_domain:string -> string -> t list
val to_string : t -> string
val to_string_utf8 : t -> string
val list_to_header_value : t list -> string
val local_part : t -> string
val set_local_part : t -> string -> t
val domain : t -> Domain.t option
val set_domain : t -> Domain.t option -> t

val address_part
  :  ?brackets:bool (** default: [false] *)
  -> ?lowercase_domain:bool
  -> t
  -> t

val address_part_string
  :  ?brackets:bool (** default: [false] *)
  -> ?lowercase_domain:bool
  -> t
  -> string

(** [set_address_part] expects an email address without prefix or angle brackets e.g.
    USER@DOMAIN. *)
val set_address_part : t -> string -> t Or_error.t

val prefix : t -> string option

(** [set_prefix] will remove angle brackets if given [None], otherwise angle brackets are
    added before the given prefix. *)
val set_prefix : t -> string option -> t

val arg_type : t Command.Arg_type.t

(* Hash and comparisons are based on the address part (local_part + domain) only. *)

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Caseless : sig
  type nonrec t = t [@@deriving sexp_of, compare ~localize, hash]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module For_test : sig
  type nonrec t = t [@@deriving sexp_of]
end

module Expert : sig
  module Parser : sig
    val email : t Angstrom.t
    val skip_whitespace : unit Angstrom.t
    val prefix : string Angstrom.t
  end
end

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io, compare ~localize, hash, sexp_grammar, stable_witness, equal ~localize]

    include
      Stable_comparable.With_stable_witness.V1
      with type t := t
      with type comparator_witness = comparator_witness

    val of_string_exn : ?default_domain:string -> string -> t
    val to_string : t -> string
  end

  module Domain : sig
    module V1 :
      Stable_comparable.With_stable_witness.V1
      with type t := Domain.t
      with type comparator_witness = Domain.comparator_witness
  end
end
