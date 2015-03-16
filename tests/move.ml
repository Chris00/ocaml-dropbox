open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only three entries in command line and that Sys.argv.(0)
    is from_path, Sys.argv.(1) is to_path and Sys.argv.(2) is the root *)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | a -> if List.length a = 3 then
          match List.nth a 2, List.nth a 1, List.nth a 0 with
          | from_path, to_path, root -> D.move t from_path to_path root
                                        >>= fun m -> Lwt_io.printlf "%s"
                                        (Dropbox_j.string_of_metadata m)
         else Lwt_io.printf ("The function must take on command line \
                              three arguments: from_path, to_path and root.\n")

let () =
  Common.run main
