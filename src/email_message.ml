module Bigstring_shared = Email_message_kernel.Bigstring_shared
module Email = Email
module Email_address = Email_address
module Email_headers = Email_message_kernel.Headers
module Email_selector = Selector
module Email_wrapper = Wrapper
module Email_date = Email_date
module Mimestring = Email_message_kernel.Mimestring
module Octet_stream = Email_message_kernel.Octet_stream
module String_monoid = String_monoid
module Lf_or_crlf = Email_message_kernel.Lf_or_crlf

module Email_message_stable = struct
  module Email = struct
    include Email.Stable
    module Raw_content = Email_message_kernel.Raw_content.Stable
    module Simple = Email_message_kernel.Simple.Stable
  end

  module Email_address = Email_address.Stable
  module Email_wrapper = Email_wrapper.Stable
  module Email_headers = Email_message_kernel.Headers.Stable
end

module Private = struct
  include Email_message_kernel.Private
end
