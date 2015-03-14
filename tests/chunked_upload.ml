open Lwt
module D = Dropbox_lwt_unix

let upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  Lwt_unix.stat fn >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let lgth_buff = 4096 in
  let numb = size / lgth_buff in
  let rest = size mod lgth_buff in
  Lwt_io.printf "size %i lgth_buff %i numb %i rest %i\n"
                 size lgth_buff numb rest >>= fun () ->
  let buffer = String.create lgth_buff in
  Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
  let stream = Cohttp_lwt_body.of_string buffer in
  D.chunked_upload t stream >>= fun chunked_upload ->
  let buffer = String.create lgth_buff in
  Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
  let stream = Cohttp_lwt_body.of_string buffer in
  D.chunked_upload t ~offset:(lgth_buff) stream >>= fun chkd_upld ->
           D.commit_chunked_upload t ~upload_id:(chkd_upld.D.upload_id) fn
           >>= fun m -> Lwt_unix.close fd >>= fun () ->
           Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)

let upload1 t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  Lwt_unix.stat fn >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let lgth_buff = 4096 in
  let numb = size / lgth_buff in
  let rest = size mod lgth_buff in
  match numb with
  | 0 -> let buffer = String.create size in
         Lwt_unix.read fd buffer 0 size >>= fun _ ->
         let stream =Cohttp_lwt_body.of_string buffer in
         D.file_put t fn size stream >>= fun metadata ->
         Lwt_unix.close fd >>= fun () -> Lwt_io.printlf "Sended %s" fn
  | _ -> let buffer = String.create lgth_buff in
         Lwt_io.printf "size %i lgth_buff %i numb %i rest %i\n"
                        size lgth_buff numb rest >>= fun () ->
         Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
         let stream = Cohttp_lwt_body.of_string buffer in
         D.chunked_upload t stream >>= fun chunked_upload ->
         for_lwt i = 1 to (numb-4) do 
           let buffer = String.create lgth_buff in
           Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
           let stream = Cohttp_lwt_body.of_string buffer in
           D.chunked_upload t ~offset:(i * lgth_buff) stream >>=
           fun _ -> return ()
         done;
         let buffer = String.create rest in
         Lwt_unix.read fd buffer 0 rest >>= fun _ ->
         let stream =Cohttp_lwt_body.of_string buffer in
         D.chunked_upload t ~offset:(numb * lgth_buff) stream >>=
         fun chkd_upld ->
         D.commit_chunked_upload t ~upload_id:(chkd_upld.D.upload_id) fn
         >>= fun m -> Lwt_unix.close fd >>= fun () ->
         Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)

let true_upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  Lwt_unix.stat fn >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let buffer = String.create size in
  ignore(Lwt_unix.read fd buffer 0 size);
  let stream =Cohttp_lwt_body.of_string buffer in
  D.chunked_upload t ~offset:size stream >>= fun chkd_upld ->
  D.commit_chunked_upload t ~upload_id:(chkd_upld.D.upload_id) fn >>= fun m ->
  Lwt_unix.close fd >>= fun () ->
  Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
