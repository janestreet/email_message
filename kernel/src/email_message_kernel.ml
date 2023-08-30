include Email (** @inline *)

module Bigstring_shared = Bigstring_shared
module Content = Email_content
module Raw_content = Email_raw_content
module Simple = Email_simple
module Headers = Headers
module Mimestring = Mimestring
module Octet_stream = Octet_stream
module String_monoid = String_monoid
module Lf_or_crlf = Lf_or_crlf

module Stable = struct
  include Email.Stable
  module Raw_content = Email_raw_content.Stable
  module Simple = Email_simple.Stable
  module Headers = Headers.Stable
end

module Private = struct
  module Boundary = Boundary
  module Media_type = Media_type
  module Rfc = Rfc
  module Encoded_word = Encoded_word

  module Email_intf = struct
    module type Email = Email_intf.Email with type t = Email.t
  end

  module Email_simple_intf = struct
    module type Email_simple =
      Email_simple_intf.Email_simple
        with type Mimetype.t = Email_simple.Mimetype.t
         and type Content.t = Email_simple.Content.t

    module type Mimetype =
      Email_simple_intf.Mimetype with type t = Email_simple.Mimetype.t

    module type Content =
      Email_simple_intf.Content
        with module Mimetype := Email_simple.Mimetype
         and type attachment_name := Email_simple.attachment_name
         and type t = Email_simple.Content.t

    module type Expert =
      Email_simple_intf.Expert
        with module Mimetype := Email_simple.Mimetype
         and module Content := Email_simple.Content
         and type attachment_name := Email_simple.attachment_name
         and type t := Email_simple.t

    module type Stable =
      Email_simple_intf.Stable
        with type Mimetype.latest := Email_simple.Mimetype.t
         and type Content.latest := Email_simple.Content.t
  end

  module String_monoid_intf = struct
    module type Underlying =
      String_monoid_intf.Underlying with type t = String_monoid.Underlying.t

    module type String_monoid =
      String_monoid_intf.String_monoid
        with type t = String_monoid.t
         and type Underlying.t = String_monoid.Underlying.t
  end
end
