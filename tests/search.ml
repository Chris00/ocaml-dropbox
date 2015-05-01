open Lwt
module D = Dropbox_lwt_unix

(** We assume there is only two entries in command line and that Sys.argv.(0)
    is the query and Sys.argv.(1) is the path to the folder we want to
    search from *)

let print_search meta_list query =
  match List.length meta_list with
  | 0 -> Lwt_io.printlf "No file or folder whose name contains %s found." query
  | _ -> let get_path m = m.D.path in
         let paths = String.concat ", " (List.map get_path meta_list) in
         Lwt_io.printlf "%s" paths

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | [query] -> D.search t query >>= fun search -> print_search search query
  | [fn;query] -> D.search t ~fn query
                  >>= fun search -> print_search search query
  | _ -> Lwt_io.printf "Error, the function must take on command line only \
                        one or two arguments the query and optionnaly the path."

let () =
  Common.run main
