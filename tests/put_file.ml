open Lwt
module D = Dropbox_lwt_unix

let upload t fn =
  let buffer = String.create 1024 in
  let cl = ref 0 in (* cl for content_length *)
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  let read () =
    Lwt_unix.read fd buffer 0 1024 >>= fun len ->
    cl := !cl + len; (* calculate the size of the file *)
    match len with
    | 1024 -> return(Some buffer)
    | 0 -> return(None)
    | a -> return(Some (String.sub buffer 0 a)) in
  let stream =Lwt_stream.from read in
  D.put_file t fn (!cl) stream >>= fun metadata ->
  Lwt_unix.close fd >>= fun () -> Lwt_io.printlf "Sended %s" fn

let main t args =
  match args with
  | [] -> upload t "test.txt"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
