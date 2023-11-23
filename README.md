"Email Message"
===============

`email_message` is a library that defines types that represent an RFC2822 email.

# Parsing an email
You can use `Email.of_string` to parse an email (e.g. from a ".eml" file).

Once you have your hands on an `Email.t`, you can use various functions in the
`Email.Simple` and `Email` modules to inspect parts of the email. For example:

- `Email.Simple.subject` : the subject of the email
- `Email.Simple.from` : the From header sender of the email
- `Email.headers` : all the email headers
- `Email.Simple.all_attachments` the email attachments

# Constructing an email
The `Email.Simple` module exposes various functions for constructing an email. For example:

```ocaml
Email.Simple.create
  ~from:(Email_address.of_string "sender@example.com")
  ~to_:[ (Email_address.of_string "recipient@example.com") ]
  ~subject:"Example email"
  (Email.Simple.Content.text_utf8 "This is an example email.")
```
