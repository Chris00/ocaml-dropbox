open Lwt
module D = Dropbox_lwt_unix

(** If there is no entry, we call delta without param.
    If there is one entry, we call delta with the param path_prefix as
       Sys.argv.(0).
    If there is two entries, we call delta with the param cursor as
       Sys.argv.(0) and the param path_prefix as the second. *)



let main t args =
  match args with
  | [] ->   D.delta t >>= fun delta ->
            Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
  | a -> if List.length a = 1 then
           D.delta t ~path_prefix:(List.hd a) >>= fun delta ->
           Lwt_io.printlf "%s" (Dropbox_j.string_of_delta delta)
         else if List.length a = 2 then
           match List.nth a 1, List.nth a 0 with
           | cursor, path_prefix -> D.delta t ~cursor ~path_prefix
                                    >>= fun delta -> Lwt_io.printlf "%s"
                                               (Dropbox_j.string_of_delta delta)
         else Lwt_io.printf ("The function must take the arguments like above")
	    
let () =
  Common.run main
