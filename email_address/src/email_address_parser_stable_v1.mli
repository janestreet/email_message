type t =
  { prefix : string option
  ; local_part : string
  ; domain : string option
  }
[@@deriving sexp_of]

val skip_whitespace : unit Angstrom.t
val prefix : string Angstrom.t
val email : t Angstrom.t
val email_only : t Angstrom.t
val email_list_only : t list Angstrom.t
