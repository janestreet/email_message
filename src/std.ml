module Bigstring_shared = Bigstring_shared
module Email = struct
  include Email
  module Content = Email_content
  module Simple = Email_simple
end
module Email_address = Email_address
module Email_headers = Headers
module Email_selector = Selector
module Email_wrapper = Wrapper
module Mimestring = Mimestring
module Octet_stream = Octet_stream
module String_monoid = String_monoid
