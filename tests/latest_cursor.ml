open Lwt
module D = Dropbox_lwt_unix

(** If there is no entry, we call latest_cursor without param.
    If there is one entry, we call latest_cursor with the param
       path_prefix as Sys.argv.(0). *)



let main t args =
  match args with
  | [] ->   D.delta t >>= fun delta ->
            Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
  | a -> if List.length a = 1 then
           D.latest_cursor ~path_prefix:(List.hd a) t >>= fun delta ->
           Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
         else Lwt_io.printf "The function must take the arguments like above"

let () =
  Common.run main
