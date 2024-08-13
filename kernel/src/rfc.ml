open Core

module RFC2045 = struct
  module Token = struct
    include (Mimestring.Case_insensitive : Mimestring.S)

    let is_valid str =
      (not (String.is_empty str))
      && String.for_all str ~f:(function
        | '('
        | ')'
        | '<'
        | '>'
        | '@'
        | ','
        | ';'
        | ':'
        | '\\'
        | '"'
        | '/'
        | '['
        | ']'
        | '?'
        | '=' -> false
        (* '\n', '\r', ' ' are excluded by the following: *)
        | '\033' .. '\126' -> true
        | _ -> false)
    ;;

    let is_valid_or_quote str = if is_valid str then str else Mimestring.quote str
  end
end
