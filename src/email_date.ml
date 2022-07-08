open! Core
module Time = Time_float_unix

let utc_offset_string time ~zone =
  let utc_offset = Time.utc_offset time ~zone in
  let is_utc = Time.Span.( = ) utc_offset Time.Span.zero in
  if is_utc
  then "Z"
  else
    String.concat
      [ (if Time.Span.( < ) utc_offset Time.Span.zero then "-" else "+")
      ; Time.Ofday.to_string_trimmed
          (Time.Ofday.of_span_since_start_of_day_exn (Time.Span.abs utc_offset))
      ]
;;

let rfc822_date now =
  let zone = force Time.Zone.local in
  let offset_string =
    utc_offset_string ~zone now |> String.filter ~f:(fun c -> Base.Char.( <> ) c ':')
  in
  let now_string = Time.format now "%a, %d %b %Y %H:%M:%S" ~zone in
  sprintf "%s %s" now_string offset_string
;;

open Angstrom
open Let_syntax

(* Folding whitespace and comments. See RFC2822#3.2.3 *)
let comment =
  let comment_text =
    skip (function
      (* slight simplification of the RFC *)
      | '(' | ')' | '\\' -> false
      | _ -> true)
  in
  let quoted_pair =
    let%map (_ : char) = char '\\' *> any_char in
    ()
  in
  fix (fun comment ->
    char '(' *> skip_many (comment_text <|> quoted_pair <|> comment) <* char ')')
;;

let single_whitespace_or_comment = comment <|> skip Char.is_whitespace
let folding_whitespace = skip_many1 single_whitespace_or_comment <?> "FWS"
let optional_folding_whitespace = skip_many single_whitespace_or_comment <?> "?FWS"

let rec skip_min_max ~min ~max thing =
  assert (min <= max);
  if min > 0
  then thing *> skip_min_max ~min:(min - 1) ~max:(max - 1) thing
  else if max > 0
  then option () (skip_min_max ~min:1 ~max thing)
  else return ()
;;

let parse_two_digit_int =
  consumed (skip_min_max (skip Char.is_digit) ~min:2 ~max:2) >>| Int.of_string
;;

let parse_two_to_four_digit_int =
  consumed (skip_min_max (skip Char.is_digit) ~min:2 ~max:4) >>| Int.of_string
;;

let parse_one_or_two_digit_int =
  consumed (skip_min_max (skip Char.is_digit) ~min:1 ~max:2) >>| Int.of_string
;;

let parse_day_of_week =
  choice (List.map Day_of_week.all ~f:(Fn.compose string_ci Day_of_week.to_string))
  <?> "day of week"
;;

let parse_month =
  choice
    (List.map Month.all ~f:(fun month ->
       const month <$> string_ci (Month.to_string month)))
  <?> "month"
;;

let parse_time_zone =
  let utc_offset =
    (let%mapn sign =
       Angstrom.choice [ const Sign.Pos <$> char '+'; const Sign.Neg <$> char '-' ]
       <?> "sign"
     and hours = parse_two_digit_int <?> "hours"
     and minutes = parse_two_digit_int <?> "minutes" in
     let utc_offset = Time.Span.create ~sign ~hr:hours ~min:minutes () in
     if not
          (Time.Span.between
             utc_offset
             ~low:(Time.Span.neg Time.Span.day)
             ~high:Time.Span.day)
     then raise_s [%message "The supplied UTC offset is semantically invalid."];
     utc_offset)
    <?> "utc offset"
  in
  let military_time_zone =
    const Time.Span.zero
    <$> (List.init 26 ~f:(fun i ->
      Char.of_int_exn (Char.to_int 'A' + i) |> Char.to_string)
         |> List.filter ~f:(String.( <> ) "J")
         |> List.map ~f:string_ci
         |> choice)
    <?> "military zone"
  in
  let obsolete_zone =
    [ "UT", Time.Span.zero
    ; "GMT", Time.Span.zero
    ; "EST", Time.Span.create ~sign:Sign.Neg ~hr:5 ()
    ; "EDT", Time.Span.create ~sign:Sign.Neg ~hr:4 ()
    ; "CST", Time.Span.create ~sign:Sign.Neg ~hr:6 ()
    ; "CDT", Time.Span.create ~sign:Sign.Neg ~hr:5 ()
    ; "MST", Time.Span.create ~sign:Sign.Neg ~hr:7 ()
    ; "MDT", Time.Span.create ~sign:Sign.Neg ~hr:6 ()
    ; "PST", Time.Span.create ~sign:Sign.Neg ~hr:8 ()
    ; "PDT", Time.Span.create ~sign:Sign.Neg ~hr:7 ()
    ]
    |> List.map ~f:(fun (abbrev, offset) -> const offset <$> string_ci abbrev)
    |> choice
       <?> "obsolote zone"
  in
  choice [ obsolete_zone; military_time_zone; utc_offset ] <?> "time zone"
;;

let untruncate_year year =
  (* As per https://tools.ietf.org/html/rfc5322#section-4.3 *)
  if year <= 49 then 2000 + year else if year <= 999 then 1900 + year else year
;;

let parse_date =
  (let%mapn () =
     option () (parse_day_of_week *> option ' ' (char ',') *> folding_whitespace)
   and day = parse_one_or_two_digit_int <?> "day" <* folding_whitespace
   and month = parse_month
   and year =
     folding_whitespace *> (parse_two_to_four_digit_int >>| untruncate_year <?> "year")
   in
   Date.create_exn ~y:year ~m:month ~d:day)
  <?> "date"
;;

let parse_time_of_day =
  (let%mapn hour = parse_two_digit_int <?> "hour"
   and minutes = char ':' *> parse_two_digit_int <?> "minute"
   and seconds = option 0 (char ':' *> parse_two_digit_int <?> "second") in
   Time.Ofday.create ~hr:hour ~min:minutes ~sec:seconds ())
  <?> "time of day"
;;

let rfc2822_date_parser =
  let%mapn date = parse_date <* folding_whitespace
  and time_of_day = parse_time_of_day <* folding_whitespace
  and utc_offset = parse_time_zone in
  let time_no_zone = Time.of_date_ofday ~zone:Time.Zone.utc date time_of_day in
  Time.sub time_no_zone utc_offset, utc_offset
;;

(* See https://tools.ietf.org/html/rfc5322#section-3.3 for the full spec. Also note
   https://tools.ietf.org/html/rfc5322#appendix-A.5 on whitespace.

   https://github.com/moment/moment/blob/022dc038af5ebafafa375f4566fb23366f4e4aa8/src/lib/create/from-string.js#L189
   (alongside the RFC), was used as a reference for this implementation. *)
let of_string_exn_with_utc_offset date =
  match
    parse_string
      ~consume:All
      (optional_folding_whitespace *> rfc2822_date_parser
       <* optional_folding_whitespace
       <* end_of_input)
      date
  with
  | Ok time_and_utc_offset -> time_and_utc_offset
  | Error message -> failwith ("Failed to parse RFC822 " ^ message)
;;

let of_string_exn date =
  let time, _utc_offset = of_string_exn_with_utc_offset date in
  time
;;

let of_string_exn_with_time_zone date =
  let time, utc_offset = of_string_exn_with_utc_offset date in
  (* In cases where the utc_offset has minutes, we might be dropping information. Let's
     just raise to avoid any possible confusion. We don't really expect to see time
     zones with a minute offset in the wild.
     FYI: India is the main non-hour time zone (IST, UTC+5:30).
     Its probably fine to punt for now, as long as this is only used with internal email flow. *)
  let utc_offset_parts = Time.Span.to_parts utc_offset in
  if utc_offset_parts.min <> 0
  then
    raise_s
      [%message
        "NOT IMPLEMENTED: Time zones with minute offsets are not yet supported (probably \
         IST/UTC+5:30)"
          (utc_offset : Time.Span.t)];
  ( time
  , Time.Zone.of_utc_offset
      ~hours:(Sign.to_int utc_offset_parts.sign * utc_offset_parts.hr) )
;;
