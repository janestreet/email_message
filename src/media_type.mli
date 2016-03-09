open! Core.Std

type t

include Stringable.S with type t := t
include String_monoidable.S with type t := t

val create : ?params:(Headers.Name.t * string) list -> string -> string -> t

val mime_type : t -> string
val mime_subtype : t -> string
val params : t -> (Headers.Name.t * string) list
val param : t -> Headers.Name.t -> string option

val is_multipart : t -> bool
val is_digest : t -> bool

val is_simple : t -> bool

    (** A composite message may be decomposed into several submessages *)
val is_composite : t -> bool

val mode : t -> [ `Text | `Binary ]

val multipart_boundary : t -> Boundary.t option

val last : Headers.t -> t option
val set_at_bottom : Headers.t -> t -> Headers.t
val default : parent:(t option) -> t
