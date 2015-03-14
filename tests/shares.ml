open Lwt
module D = Dropbox_lwt_unix

let shares t fn =
  D.shares t fn >>= fun shares ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_shares shares)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (shares t) args

let () =
  Common.run main
