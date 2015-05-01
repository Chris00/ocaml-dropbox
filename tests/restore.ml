open Lwt
module D = Dropbox_lwt_unix

(** Return the metadata of the first element of the metadata list returned
    by revisions function on a given [filename]. *)

let restore t fn =
  D.revisions t fn >>= (function
  | Some metadata_list -> D.restore t (List.hd metadata_list) >>= (function
    | Some meta -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata meta)
    | None -> Lwt_io.printlf "%s is not a file" fn)
  | None -> Lwt_io.printlf "No file %s" fn)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (restore t) args

let () =
  Common.run main
