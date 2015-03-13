open Lwt
module D = Dropbox_lwt_unix

let main t args =
  D.delta_latest_cursor t >>= fun delta ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
	    
let () =
  Common.run main
