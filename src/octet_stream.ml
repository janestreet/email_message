open Core

module Encoding = struct
  include Octet_stream0.Encoding0

  let of_headers ?(ignore_base64_for_multipart = true) headers : t option =
    Headers.last headers "content-transfer-encoding"
    |> Option.map ~f:of_string
    |> function
    | Some `Base64 when ignore_base64_for_multipart ->
      let is_multipart =
        match Media_type.from_headers headers with
        | Some media_type -> Media_type.is_multipart media_type
        | None -> false
      in
      if is_multipart then Some (default :> t) else Some `Base64
    | _ as encoding -> encoding
  ;;

  let of_headers_or_default ?ignore_base64_for_multipart headers =
    match of_headers ?ignore_base64_for_multipart headers with
    | Some t -> t
    | None -> (default :> t)
  ;;
end

include Octet_stream0
