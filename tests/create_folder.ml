open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
is the path of the folder and Sys.argv.(1) is the root *)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | a -> if List.length a = 2 then
           match List.nth a 1, List.nth a 0 with
           | path, root -> D.create_folder t path root >>= fun m ->
                     Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
         else Lwt_io.printf ("The function must take on command line \
                              only two arguments: the root and the path.\n")

let () =
  Common.run main
