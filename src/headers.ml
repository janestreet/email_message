open Core.Std

type 'a field_list = 'a Field_list.t with sexp, bin_io, compare
include (Field_list
         : module type of Field_list with type 'a t := 'a field_list)
type t = string field_list with sexp, bin_io, compare

let to_string_monoid t =
  let field_to_string_monoid ((name : Field_name.t), body) =
    String_monoid.concat_string [(name :> string); ":"; body; "\n"]
  in
  String_monoid.concat
    (List.map t ~f:field_to_string_monoid)

let empty = []

let add t ~name ~value =
  add t ~name (" " ^ value)
let add_at_bottom t ~name ~value =
  add_at_bottom t ~name (" " ^ value)
let set t ~name ~value =
  set t ~name (" " ^ value)
let set_at_bottom t ~name ~value =
  set_at_bottom t ~name (" " ^ value)

module Content_type = struct
  module Token = Rfc.RFC2045.Token

  let last t =
    Option.bind (Field_list.last t "content-type")
      (fun field -> Option.try_with (fun () -> Media_type.of_string field))

  let default_default =
    { Media_type.
      mime_type = Token.of_string "text";
      mime_subtype = Token.of_string "plain";
      params = [(Field_name.of_string "charset","us-ascii")]
    }
  ;;

  let default_digest =
    { Media_type.
      mime_type = Token.of_string "message";
      mime_subtype = Token.of_string "rfc2822";
      params = []
    }
  ;;

  let default ~parent =
    if Option.value_map parent ~f:Media_type.is_digest ~default:false
    then default_digest
    else default_default
  ;;
end
