open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the path and Sys.argv.(1) is the root *)

let string_to_root a = match a with
  | "auto" -> `Auto
  | "dropbox" -> `Dropbox
  | "sandbox" -> `Sandbox
  | _ -> invalid_arg "root must be auto, dropbox or sandbox"

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | [path; root] -> D.Fileops.delete t ~root:(string_to_root root) path
      >>= (function
      | Some m -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
      | None -> Lwt_io.printlf "No file %s" path)
  | _ -> Lwt_io.printf "The function must take on command line \
                        two arguments: the path and the root.\n"

let () =
  Common.run main
