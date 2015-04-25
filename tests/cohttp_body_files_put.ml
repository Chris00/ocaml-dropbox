open Lwt
module D = Dropbox_lwt_unix

let upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd -> Lwt_unix.stat fn
  >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let buffer = Bytes.create size in
  Lwt_unix.read fd buffer 0 size >>= fun _ ->
  let stream =Cohttp_lwt_body.of_string (Bytes.to_string buffer) in
  D.cohttp_body_files_put t fn size stream
  >>= fun m -> Lwt_unix.close fd
  >>= fun () -> Lwt_io.printlf "Sended: %s\nMetadata: %s\n"
                fn (Dropbox_j.string_of_metadata m)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
