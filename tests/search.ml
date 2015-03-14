open Lwt
module D = Dropbox_lwt_unix

  (** We assume there is only two entries in command line and that Sys.argv.(0)
      is the path of the file and Sys.argv.(1) is the rev of the file *)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | a -> if List.length a = 1 then
           match List.nth a 0 with
           | x -> D.search t x >>= fun search ->
                  Lwt_io.printlf "%s" (Dropbox_j.string_of_search search)
         else if List.length a = 2 then
           match List.nth a 0, List.nth a 1 with
           | x, y -> D.search t x ~fn:y >>= fun search ->
                     Lwt_io.printlf "%s" (Dropbox_j.string_of_search search)
         else Lwt_io.printf ("Error, the function must take on command line \
           only one or two arguments the query and optionnaly the path.\n")

let () =
  Common.run main
