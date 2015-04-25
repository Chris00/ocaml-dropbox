open Lwt
module D = Dropbox_lwt_unix

(** Only display maximum two revisions for the file fn. *)

let revisions t fn =
  D.revisions t fn >>= fun revi ->
  if List.length revi < 2 then
    Lwt_io.printlf "%s" (Dropbox_j.string_of_revisions revi)
  else
    Lwt_io.printlf "Revisions for the file %s:\nrev 1: %s\nrev 2: %s"
      fn (List.nth revi 0).D.rev (List.nth revi 1).D.rev

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (revisions t) args

let () =
  Common.run main
