open Core.Std

type 'a t = (Field_name.t * 'a) list with sexp, bin_io, compare

let hash t =
  Common.list_hash t ~hash:Hashtbl.hash

let last t name =
  let name = Field_name.of_string name in
  List.Assoc.find ~equal:Field_name.equal (List.rev t) name

let add t ~name value =
  let name = Field_name.of_string name in
  let rec add acc = function
    | ((name', _) :: _) as fields when Field_name.equal name name' ->
      List.rev acc @ [name, value] @ fields
    | field :: fields ->
      add (field :: acc) fields
    | [] ->
      (name, value) :: t
  in
  add [] t

let add_at_bottom t ~name value =
  List.rev (add (List.rev t) ~name value)

let set t ~name value =
  let name = Field_name.of_string name in
  let rec set acc = function
    | ((name', _) :: fields) when Field_name.equal name name' ->
      List.rev acc @ [name, value] @ fields
    | field :: fields ->
      set (field :: acc) fields
    | [] ->
      (name, value) :: t
  in
  set [] t

let set_at_bottom t ~name value =
  List.rev (set (List.rev t) ~name value)

TEST_MODULE = struct
  type field_list = string t
  type t = (string * string) list with sexp, compare

  let of_strings =
    List.map ~f:(fun (name, value) -> (Field_name.of_string name, value))

  let to_strings (t : field_list) = (t :> t)

  let add t ~name value = add t ~name value |> to_strings

  let add_at_bottom t ~name value =
    add_at_bottom t ~name value |> to_strings

  let set t ~name value =
    set t ~name value |> to_strings

  let set_at_bottom t ~name value =
    set_at_bottom t ~name value |> to_strings

  let t = [ "A", "a1" ; "B", "b1" ; "B", "b2" ]
          |> of_strings

  TEST_UNIT =
    <:test_result<t>> (add t ~name:"B" "b3")
      ~expect:[ "A", "a1" ; "B", "b3"; "B", "b1" ; "B", "b2" ]

  TEST_UNIT =
    <:test_result<t>> (add t ~name:"C" "c1")
      ~expect:[ "C", "c1"; "A", "a1" ; "B", "b1" ; "B", "b2" ]

  TEST_UNIT =
    <:test_result<t>> (set t ~name:"B" "b3")
      ~expect:[ "A", "a1" ; "B", "b3"; "B", "b2" ]

  TEST_UNIT =
    <:test_result<t>> (set t ~name:"b" "b3")
      ~expect:[ "A", "a1" ; "b", "b3"; "B", "b2" ]

  TEST_UNIT =
    <:test_result<t>> (set t ~name:"C" "c1")
      ~expect:[ "C", "c1"; "A", "a1" ; "B", "b1" ; "B", "b2" ]

  TEST_UNIT =
    <:test_result<t>> (set t ~name:"c" "c1")
      ~expect:[ "c", "c1"; "A", "a1" ; "B", "b1" ; "B", "b2" ]

  TEST_UNIT =
    <:test_result<t>> (add_at_bottom t ~name:"B" "b3")
      ~expect:[ "A", "a1" ; "B", "b1" ; "B", "b2"; "B", "b3" ]

  TEST_UNIT =
    <:test_result<t>> (add_at_bottom t ~name:"C" "c1")
      ~expect:[ "A", "a1" ; "B", "b1" ; "B", "b2"; "C", "c1" ]

  TEST_UNIT =
    <:test_result<t>> (set_at_bottom t ~name:"B" "b3")
      ~expect:[ "A", "a1" ; "B", "b1"; "B", "b3" ]

  TEST_UNIT =
    <:test_result<t>> (set_at_bottom t ~name:"C" "c1")
      ~expect:[ "A", "a1" ; "B", "b1" ; "B", "b2"; "C", "c1" ]
end

let find_all t name =
  let name = Field_name.of_string name in
  List.filter_map t ~f:(fun (name', value) ->
    if Field_name.equal name name' then Some value else None)

let names t =
  List.map t ~f:fst
