include String_monoid_intf.String_monoid (** @inline *)

module Private : sig
  val output : dst_output:(Underlying.t -> unit) -> t -> unit
end
