open Lwt
module D = Dropbox_lwt_unix

let upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  let read () =
    let buffer = String.create 1024 in
    Lwt_unix.read fd buffer 0 1024 >>= fun len ->
    match len with
    | 1024 -> return(Some buffer)
    | 0 -> return(None)
    | a -> return(Some (String.sub buffer 0 (a))) in
  let stream =Lwt_stream.from read in
  Lwt_unix.stat fn >>= fun u -> return(u.st_size) >>= fun size ->
  D.put_file t fn size stream >>= fun metadata ->
  Lwt_unix.close fd >>= fun () -> Lwt_io.printlf "Sended %s" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
