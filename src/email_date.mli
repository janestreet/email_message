open! Core

(** Generate an RFC822-style date *)
val rfc822_date : ?zone:Time_float.Zone.t -> Time_float.t -> string

(** Parse an RFC822-style string into a [Time.t].

    Note that if the weekday is provided, it will not be semantically validated. *)
val of_string_exn : string -> Time_float.t

(** Similar to [of_string_exn], but also return back the time zone that was parsed from
    the provided string. *)
val of_string_exn_with_time_zone : string -> Time_float.t * Time_float.Zone.t
