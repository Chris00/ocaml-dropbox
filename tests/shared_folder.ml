open Lwt
module D = Dropbox_lwt_unix

let shared_folders t =
  D.shared_folders t >>= fun shared_folders ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_shared_folders shared_folders)

let shared_folder t id =
  D.shared_folder t ~shared_folder_id:id >>= fun shared_folder ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_shared_folder shared_folder)

let main t args =
  match args with
  | [] -> shared_folders t
  | _ -> Lwt_list.iter_p (shared_folder t) args

let () =
  Common.run main
