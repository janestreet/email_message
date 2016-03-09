open! Core.Std
module Whitespace : sig
  type t =
    [ `Keep (* Leave whitespace unchanged *)
    | `Strip (* Cleanup leading and trailing whitespace on each line *)
    ] [@@deriving sexp]
  val default : t (* `Strip *)
end

module Name : sig
  (* Case insensitive *)
  type t = string [@@deriving sexp, bin_io]

  val of_string : string -> t
  val to_string : t -> string

  include Comparable.S with type t := t

(* Short hand for [let is a b = equal a (of_string b)] *)
  val is : t -> string -> bool
end

module Value : sig
  type t = string [@@deriving sexp, bin_io]

(** Normalize the whitespace for processing/
    if [whitespace == `Keep] this does nothing.
    if [whitespace = `Strip] (default), strip leading/trailing whitespace on every line. *)
  val of_string : ?whitespace:Whitespace.t -> string -> t
  (** Normalize the whitespace for transport (insert the appropriate leading space).
      if [whitespace == `Keep] this does nothing.
      if [whitespace == `Strip] (default), insert a leading space and indent subsequent
        lines with a tab (remove any other leading/trailing space on every line).
      *)
  val to_string : ?whitespace:Whitespace.t -> t -> string

  include Comparable.S with type t := t
end


(* The add and set functions are same as in Field_list, except they add a space
   before the value. *)
type t [@@deriving sexp, bin_io, compare]

include String_monoidable.S with type t := t
val to_string : t -> string

val hash : t -> int

val empty : t
val append : t -> t -> t

val of_list : whitespace:Whitespace.t -> (Name.t * Value.t) list -> t
val to_list : ?whitespace:Whitespace.t -> t -> (Name.t * Value.t) list

val last : ?whitespace:Whitespace.t -> t -> Name.t -> Value.t option
val find_all : ?whitespace:Whitespace.t -> t -> Name.t -> Value.t list
val names : t -> Name.t list

val add : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t
val add_all : ?whitespace:Whitespace.t -> t -> (Name.t * Value.t) list -> t
val add_at_bottom : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t
val add_all_at_bottom : ?whitespace:Whitespace.t -> t -> (Name.t * Value.t) list -> t

val set : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t
val set_at_bottom : ?whitespace:Whitespace.t -> t -> name:Name.t -> value:Value.t -> t

val filter : ?whitespace:Whitespace.t -> t -> f:(name:Name.t -> value:Value.t -> bool) -> t

(** rewrite header values, preserving original whitespace where possible.

    [whitespace] is used to [Value.of_string ?whitespace] the [~value] before passing to [f],
    and again to [Value.to_string ?whitespace] the result.
    If the [~value] and [f ~name ~value] are the same no change will be made (white space is preserved).

    Particularly the following is an identity transform:
    [ map ~whitespace:`Strip ~f:(fun ~name:_ ~value -> Value.of_string ~whitespace:`Strip value) ].
    By contrast the following will 'normalize' the whitespace on all headers.
    [ map ~whitespace:`Keep ~f:(fun ~name:_ ~value -> Value.of_string ~whitespace:`Strip value) ].
*)
val map : ?whitespace:Whitespace.t -> t -> f:(name:Name.t -> value:Value.t -> Value.t) -> t
