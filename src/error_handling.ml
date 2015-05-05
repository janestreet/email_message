open Core.Std

let ok_or_stderr res ?line ~on_error =
  match res with
  | Result.Ok a -> a
  | Result.Error b ->
    begin
    match line with
    | Some n -> eprintf "Line %d: " n
    | None   -> ()
    end;
    eprintf "%s\n%!" b;
    on_error

