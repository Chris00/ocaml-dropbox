open Lwt
module D = Dropbox_lwt_unix

let upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd -> Lwt_unix.stat fn
  >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let read () =
    let buffer = Bytes.create 1024 in
    Lwt_unix.read fd buffer 0 1024 >>= fun len ->
    match len with
    | 1024 -> return(Some (Bytes.to_string buffer ))
    | 0 -> return(None)
    | a -> return(Some (Bytes.to_string (Bytes.sub buffer 0 a))) in
  D.files_put t fn (`Stream (Lwt_stream.from read))
  >>= fun meta -> Lwt_unix.close fd
  >>= fun () -> Lwt_io.printlf "Send: %s\nMetadata: \n%s"
                fn (Dropbox_j.string_of_metadata meta)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
