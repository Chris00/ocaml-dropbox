open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only three entries in command line and that Sys.argv.(0)
    is the from_path, Sys.argv.(1) is the to_path and Sys.argv.(2) the root.
    from_copy_ref not tested yet. *)

let string_to_root a = match a with
  | "auto" -> `Auto
  | "dropbox" -> `Dropbox
  | "sandbox" -> `Sandbox
  | _ -> invalid_arg "root must be auto, dropbox or sandbox"

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | a -> if List.length a = 3 then
          match List.nth a 2, List.nth a 1, List.nth a 0 with
          | from, to_path, root -> D.copy t ~from_path:from to_path 
                                   (string_to_root root)
                                   >>= fun m -> Lwt_io.printlf "%s"
                                   (Dropbox_j.string_of_metadata m)
         else Lwt_io.printf "The function must take on command line \
                             two arguments: the path and the root.\n"

let () =
  Common.run main
