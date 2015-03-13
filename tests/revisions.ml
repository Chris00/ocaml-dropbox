open Lwt
module D = Dropbox_lwt_unix

let revisions t fn =
  D.revisions t fn >>= fun rev ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_revisions rev)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (revisions t) args

let () =
  Common.run main
