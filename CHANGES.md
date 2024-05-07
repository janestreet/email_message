## Release v0.17.0

- `Email_address` module now has a `to_string_utf8` function to generate a UTF-8 encoded
  representation of an email address

- Split [Email_message] into [Email_message] and [Email_message_kernel]. The
  [Email_message_kernel] library is compatible with JS of OCaml.
  
- Tweak the normalization of encoded header values. We previously decoded before
  normalizing, now we normalize before decoding.
  
- Improve parsing of email addresses to catch more invalid addresses as errors

## Release v0.16.0

- `Email_simple.all_attachments` and `Email_simple.map_attachments`: Added optional
  `include_inline_parts` parameter to control whether to attempt to interpret inline
   parts as attachments

- `Email_date.rfc822_date` now accepts an optional `zone` parameter which determines what
   timezone to use in the output string

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## 113.24.00

- Bugfixes and minor API improvements.

## 113.00.00

- Extended and improved Email_message API.

## 112.17.00

Moved from janestreet-alpha

