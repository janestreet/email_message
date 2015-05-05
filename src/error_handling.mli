open Core.Std

val ok_or_stderr : ('a, string) Result.t -> ?line:int -> on_error:'a -> 'a
