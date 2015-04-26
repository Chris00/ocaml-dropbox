open Lwt
module D = Dropbox_lwt_unix

  (** We assume there is only two entries in command line and that Sys.argv.(0)
      is the query and Sys.argv.(1) is the path to the folder we want to
      search from *)

let main t args =
  if args = [] then Lwt_io.printlf "No file or folder specified"
  else if List.length args = 1 then
    D.search t (List.nth args 0)
    >>= fun search -> Lwt_io.printlf "%s" (Dropbox_j.string_of_search search)
  else if List.length args = 2 then
    match List.nth args 1, List.nth args 0 with
    | query, fn -> D.search t ~fn query >>= fun search ->
                   Lwt_io.printlf "%s" (Dropbox_j.string_of_search search)
  else
    Lwt_io.printf ("Error, the function must take on command line only one or \
                    two arguments the query and optionnaly the path.")

let () =
  Common.run main
