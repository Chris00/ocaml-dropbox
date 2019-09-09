open Lwt
module D = Dropbox_lwt_unix

let main t _ =
  D.delta t >>= fun delta ->
  D.longpoll_delta t delta.D.cursor >>= fun delta ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_longpoll_delta delta)

let () =
  Common.run main
