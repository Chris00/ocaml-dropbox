open Lwt
module D = Dropbox_lwt_unix

  (** We assume there is only two entries in command line and that Sys.argv.(0)
      is the path of the file and Sys.argv.(1) is the rev of the file.
      You can get a rev of the file by using ./run revisions.native args . *)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | a -> if List.length a = 2 then
           match List.nth a 0 , List.nth a 1 with
           | x , y -> D.restore t x y >>= fun metadata ->
                      Lwt_io.printlf "%s"
                      (Dropbox_j.string_of_metadata metadata)
	 else Lwt_io.printf "Error, the function must take on command line \
           two arguments: the past of the file and the rev."

let () =
  Common.run main
