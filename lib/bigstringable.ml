open Core.Std
module type S = sig
  type t
  val of_bigstring : Bigstring.t -> t
  val to_bigstring : t -> Bigstring.t
end
