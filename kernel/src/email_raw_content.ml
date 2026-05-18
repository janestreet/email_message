module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    (* [Bigstring_shared.t] (and thus [Email_raw_content]) is semantically treated as
       immutable, but is backed by a mutable [Bigstring.t] so the compiler cannot verify
       that it crosses contention. We use [@@unsafe_allow_any_mode_crossing] to assert
       this. *)
    type t : value mod contended portable = { raw : Bigstring_shared.Stable.V1.t option }
    [@@unboxed] [@@deriving compare, equal] [@@unsafe_allow_any_mode_crossing]

    (* Preserve sexp and bin_io representations as a plain option, not a record, so that
       the serialization format is unchanged from the original [type t = ... option]. We
       use [V1] rather than [V2] because [V2] wraps the bin_shape with a caller_identity
       UUID, which would change the bin_digest. *)
    include
      Binable.Of_binable.V1 [@alert "-legacy"]
        (struct
          type t = Bigstring_shared.Stable.V1.t option [@@deriving bin_io]
        end)
        (struct
          type nonrec t = t

          let to_binable { raw } = raw
          let of_binable raw = { raw }
        end)

    let sexp_of_t { raw } = [%sexp_of: Bigstring_shared.Stable.V1.t option] raw
    let t_of_sexp sexp = { raw = [%of_sexp: Bigstring_shared.Stable.V1.t option] sexp }

    let%expect_test "bin representation matches Bigstring_shared option" =
      print_endline [%bin_digest: t];
      print_endline [%bin_digest: Bigstring_shared.Stable.V1.t option];
      [%expect
        {|
        b1e38d087f9fdb79f246c435ac243e64
        b1e38d087f9fdb79f246c435ac243e64
        |}]
    ;;
  end
end

open! Core

(* See comment on [Stable.V1.t] for why [@@unsafe_allow_any_mode_crossing] is needed. *)
type t : value mod contended portable = Stable.V1.t = { raw : Bigstring_shared.t option }
[@@unboxed] [@@deriving compare, hash, equal] [@@unsafe_allow_any_mode_crossing]

let sexp_of_t { raw } = [%sexp_of: Bigstring_shared.t option] raw
let of_bigstring_shared bstr = { raw = Some bstr }
let of_string str = of_bigstring_shared (Bigstring_shared.of_string str)

let to_bigstring_shared { raw } =
  match raw with
  | None -> Bigstring_shared.empty
  | Some bstr -> bstr
;;

let length t = Bigstring_shared.length (to_bigstring_shared t)

module Expert = struct
  let of_bigstring_shared_option raw = { raw }
  let to_bigstring_shared_option { raw } = raw
end
