module Debug_in_this_directory = Debug
open Core.Std
open Core_extended.Std
module Debug = Debug_in_this_directory


type t = string with sexp, bin_io, compare
let create = Fn.id

let hash = String.hash

module Generator = struct
  let bcharnospace =
    "abcdefghijklmnopqrstuvwxyz" ^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ^
    "0123456789" ^
    "'()+_,-./:=?"
  ;;
  let bcharnospace_len = String.length bcharnospace

  let __UNUSED_VALUE__min_len = 1
  let max_len = 70
  let suffix_len = 20
  let base_len = max_len - suffix_len

  let __UNUSED_VALUE__max_tries = 1000

  let init_random_once = Lazy_m.of_fun (fun () -> Random.self_init ())

  let random_seq len =
    Lazy_m.force init_random_once;
    let str = String.create len in
    let rec loop pos =
      if pos < len then
      begin
        str.[pos] <- bcharnospace.[Random.int bcharnospace_len];
        loop (pos + 1)
      end
      else
        str
    in
    loop 0
  ;;

  (* This prefix guarantees that the boundary will not apear in
    - Headers
    - Quoted-printable text
    - Base64 encoded content.

    The only posibility is that it might appear in plaintext, but
    that would be incredibly rare when using a good random number
    generator.
    *)
  let prefix = "--=_::"
  let prefix_len = String.length prefix

  let generate_raw ?(validate=(Fn.const true)) len =
    let rec generate () =
      let boundary = prefix ^ (random_seq (len - prefix_len)) in
      if validate boundary then
        generate ()
      else
        boundary
    in
    generate ()
  ;;

  let generate ?text ?suggest () =
    ignore text;
    match suggest with
    | Some suggestion -> suggestion
    | None -> create (generate_raw max_len)
  ;;

  let generate_list ?text () =
    ignore text;
    let base = generate_raw base_len in
    Lazy_sequence.init
      (fun i -> Some (create (sprintf "%s::%016x" base i)))
  ;;
end

let generate = Generator.generate;;
let generate_list = Generator.generate_list;;


module Open = struct

  let to_string_monoid t =
    String_monoid.concat_string ["\n"; "--"; t; "\n"]
  ;;

end

module Close = struct

  let to_string_monoid t =
    String_monoid.concat_string ["\n"; "--"; t; "--"]
  ;;
end

module Open_first = struct

  let to_string_monoid t = String_monoid.concat_string ["--"; t; "\n"];;

end

let of_string = Fn.id;;
let to_string = Fn.id;;

let __UNUSED_VALUE__of_octet_streams _l = generate ();;

let split t bstr =
  let lexbuf = Bigstring_shared.to_lexbuf bstr in
  let rec loop pos acc has_prologue =
    let sub ?stop () =
      let len = Option.map stop ~f:(fun stop -> stop - pos) in
      Bigstring_shared.sub ~pos ?len bstr
    in
    match Lexer.find_boundary t lexbuf with
    | `Open_boundary_first pos ->
      loop pos acc false
    | `Open_boundary (stop, pos) ->
      let chunk = sub ~stop () in
      loop pos (chunk :: acc) has_prologue
    | `Close_boundary (stop, pos) ->
      let chunk = sub ~stop () in
      let epilogue =
        if pos < Bigstring_shared.length bstr then
          Some (Bigstring_shared.sub ~pos bstr)
        else
          None
      in
      (chunk :: acc, epilogue, has_prologue)
    | `Eof ->
      Debug.run_debug (fun () -> eprintf "Warning: No close boundary found\n");
      let chunk = sub () in (chunk :: acc, None, has_prologue)
  in
  (* RFC 2046: A multipart body may have a prologue and an epilogue *)
  let parts, epilogue, has_prologue = (loop 0 [] true) in
  match List.rev parts with
  | [] -> (Some bstr, [], epilogue)
  | (prologue :: parts) when has_prologue ->
    (Some prologue, parts, epilogue)
  | parts ->
    (None, parts, epilogue)
;;
