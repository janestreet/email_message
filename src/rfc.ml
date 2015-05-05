include Core.Std

(*
module RFC2822 = struct
  let unfold str =
    let lexbuf = Lexing.from_string str in
    let bigbuffer = Bigbuffer.create (String.length str) in
    Lexer.field_unstructured_unfold bigbuffer lexbuf;
    Bigbuffer.contents bigbuffer
  ;;

  let fold str =
    let lexbuf = Lexing.from_string str in
    let bigbuffer = Bigbuffer.create (String.length str) in
    Lexer.field_unstructured_fold bigbuffer lexbuf;
    Bigbuffer.contents bigbuffer
  ;;

  TEST_MODULE "Folding_and_unfolding" = struct
    TEST = (fold "a\n b\nc") = " a\n b\n c"
    TEST = (fold " a\n b\nc") = " a\n b\n c"
    TEST = (fold "\ta\n b\nc") = "\ta\n b\n c"
    TEST = (unfold " a\n b\nc d") = "a b c d"
  end
end
*)

module RFC2045 = struct
  module Token = struct
    include (Mimestring.Case_insensitive : Mimestring.S)

    let is_valid str = Lexer.is_rfc2045_token (Lexing.from_string str)
    let is_valid_or_quote str =
      if is_valid str then str
      else Mimestring.quote str

    TEST_MODULE "RFC2045.Token" = struct
      let (=) = String.(=)
      TEST = is_valid_or_quote "abcdefghijkl" = "abcdefghijkl"
      TEST = is_valid_or_quote "abc=dka" = "\"abc=dka\""
      TEST = is_valid_or_quote "" = "\"\""
      TEST = is_valid_or_quote "\"" = "\"\\\"\""
    end
  end
end

