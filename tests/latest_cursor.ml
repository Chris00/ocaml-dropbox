open Lwt
module D = Dropbox_lwt_unix

(** If there is no entry, we call latest_cursor without param.
    If there is one entry, we call latest_cursor with the param
       path_prefix as Sys.argv.(0). *)

let main t args =
  match args with
  | [] -> D.latest_cursor t >>= fun delta -> Lwt_io.printlf "%s"
                                (Dropbox_j.string_of_latest_cursor delta)
  | [path_prefix] -> D.latest_cursor ~path_prefix t >>= fun delta ->
      Lwt_io.printlf "%s" (Dropbox_j.string_of_latest_cursor delta)
  | _ -> Lwt_io.printf "The function must take the arguments like above"

let () =
  Common.run main
