open Lwt
module D = Dropbox_lwt_unix

(** If there is no argument, return the list of all shared folders.
    Otherwise, put [shared_folder_id] to get the metadata for a specific
    shared folder. *)

let shared_folders t shared_folder_id =
  let get_meta =
    if shared_folder_id <> "" then D.shared_folders ~shared_folder_id t
    else D.shared_folders t in
  get_meta >>= fun shared_folder -> match shared_folder with
    | `Singleton shared_folder -> Lwt_io.printlf "%s"
      (Dropbox_j.string_of_shared_folder shared_folder)
    | `List shared_folder -> Lwt_io.printlf "%s"
      (Dropbox_j.string_of_shared_folders shared_folder)

let main t args =
  match args with
  | [] -> shared_folders t ""
  | _ -> Lwt_list.iter_p (shared_folders t) args

let () =
  Common.run main
