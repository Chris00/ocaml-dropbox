open Lwt
module D = Dropbox_lwt_unix


let main t args =
  match args with
  | [] -> D.delta t >>= fun delta ->
          Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
  | [path] ->
     D.latest_cursor ~path_prefix:path t >>= fun delta ->
     Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
  | _ -> Lwt_io.printf "The function must take 0 or 1 argument"

let () =
  Common.run main
