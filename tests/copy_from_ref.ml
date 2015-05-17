open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the from_copy_ref, Sys.argv.(1) is the to_path *)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | [copy_ref; to_path] -> D.Fileops.copy t (`From_copy_ref copy_ref) to_path
      >>= (function
      | Some m -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
      | None -> Lwt_io.printlf "No such from_path as %s" copy_ref)
  | _ -> Lwt_io.printf "%s [from_copy_ref] [to_path]\n" Sys.argv.(0)

let () =
  Common.run main
