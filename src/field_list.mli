
(* Field names are case-insensitive. *)

type 'a t = (Field_name.t * 'a) list [@@deriving sexp, bin_io, compare]

val hash : _ t -> int

val last : 'a t -> string -> 'a option

(* If a field of the same name exists add the new field just before
   that. Otherwise add the new field at the top. *)
val add : 'a t -> name:string -> 'a -> 'a t
(* Same as add, with top and bottom reversed. *)
val add_at_bottom : 'a t -> name:string -> 'a -> 'a t
(* Replace the first occurrence of the field, or add a new field at the top. *)
val set : 'a t -> name:string -> 'a -> 'a t
(* Same as set, with top and bottom reversed. *)
val set_at_bottom : 'a t -> name:string -> 'a -> 'a t

val find_all : 'a t -> string -> 'a list

val names : 'a t -> Field_name.t list
