open Lwt
module D = Dropbox_lwt_unix

let longpoll t cursor =
  (D.longpoll_delta t cursor ) >>= fun delta ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_longpoll_delta delta)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No cursor specified"
  | _ -> Lwt_list.iter_p (longpoll t) args
	    
let () =
  Common.run main
