open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the path of the file and Sys.argv.(1) is the rev of the file.
    You can get a rev of the file by using ./run revisions.native args . *)

let main t args =
  if args = [] then Lwt_io.printlf "No file or folder specified"
  else if List.length args = 2 then
    match List.nth args 0 , List.nth args 1 with
      | rev, fn -> D.restore t rev fn >>= function
                   | Some metadata -> Lwt_io.printlf "%s"
                                      (Dropbox_j.string_of_metadata metadata)
                   | None -> Lwt_io.printlf "No file %s" fn
  else Lwt_io.printf "Error, the function must take on command line \
                      two arguments: the path of the file and the rev.\n"

let () =
  Common.run main
