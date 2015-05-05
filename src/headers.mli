open Core.Std

type 'a field_list = 'a Field_list.t with sexp, bin_io, compare
(* The add and set functions are same as in Field_list, except they add a space
   before the value. *)
include module type of Field_list with type 'a t := 'a field_list
type t = string field_list with sexp, bin_io, compare
include String_monoidable.S with type t := t

val empty : t

(* Same as functions in Field_list, except they add a space before the value. *)
val add : t -> name:string -> string -> t
val add_at_bottom : t -> name:string -> string -> t
val set : t -> name:string -> string -> t
val set_at_bottom : t -> name:string -> string -> t

(** Accesses "Content-type" fields *)
module Content_type : sig
  val last : t -> Media_type.t option
  val default : parent:(Media_type.t option) -> Media_type.t
end
