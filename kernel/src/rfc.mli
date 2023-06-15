(** Some functions for processing tokens from
    the BNF grammars that appear in the RFCs *)

module RFC2045 : sig
  module Token : sig
    include Mimestring.S

    (** True if the string doesn't need to be quoted *)
    val is_valid : string -> bool

    (** Quotes a string if necessary *)
    val is_valid_or_quote : string -> string
  end
end
