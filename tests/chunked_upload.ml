open Lwt
module D = Dropbox_lwt_unix

let create_list i =
  let list = ref [] in
  for j = 0 to i do
    list := abs(j - i) :: !list;
  done;
  !list ;;

let upload t fn =
  (** We first open the file and get its size *)
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  Lwt_unix.stat fn >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let lgth_buff = 4194304 in
  (** If the size of the file is < 4Mo, we use cohttp_body_files_put function,
      otherwise chunked_upload function *)
  if size < lgth_buff then
    let buffer = String.create size in
    Lwt_unix.read fd buffer 0 size >>= fun _ ->
    let stream = Cohttp_lwt_body.of_string buffer in
    D.cohttp_body_files_put t fn size stream
    >>= fun m -> Lwt_unix.close fd
    >>= fun () -> Lwt_io.printlf "Sended: %s\nMetadata:\n%s"
                  fn (Dropbox_j.string_of_metadata m)
  else (** The size is > 4Mo *)
    let numb = size / lgth_buff in
    let buffer = String.create lgth_buff in
    let upload_id = ref "" in
    (** We use the serialization of Lwt_list.iter_s to create our chunk
        upload (require a list, so we create a list [0;1;2;3;...;numb]
        When s = 0, we request the first chunk of the file without setting
        upload_id, and receive an upload_id. Until num = numb, we request
        each following chunk by parameting the offset and the upload_id.
        Then (num = numb), we request the last chunk whos size is < lgt_buff *)
        let read s = match s with
      | 0 -> Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
             let stream = Cohttp_lwt_body.of_string buffer in
             D.chunked_upload t stream >>= fun chunked_upload ->
             upload_id := (chunked_upload.D.upload_id);
             Lwt_io.printlf "\nUpload_id: %s" (!upload_id)
             >>= fun () -> return ()
      | num -> if num = numb then
                 Lwt_io.printlf "Offset: %i \nEnd of file" (num * lgth_buff)
                 >>= fun () -> Lwt_unix.read fd buffer 0 lgth_buff
                 >>= fun size ->
                 let stream = Cohttp_lwt_body.of_string
                   (String.sub buffer 0 size) in
                 D.chunked_upload t ~upload_id:(!upload_id)
                   ~offset:(num * lgth_buff) stream
                 >>= fun chunked_upload -> return ()
               else
                 Lwt_io.printlf "Offset: %i" (num * lgth_buff)
                 >>= fun () -> Lwt_unix.read fd buffer 0 lgth_buff
                 >>= fun _ ->
                 let stream = Cohttp_lwt_body.of_string buffer in
                 D.chunked_upload t ~upload_id:(!upload_id)
                   ~offset:(num * lgth_buff) stream
                 >>= fun _ -> return()
        in
    (** Finally, we finish the upload of the file by using
        commit_chunked_upload *)
    Lwt_list.iter_s read (create_list numb)
    >>= fun () -> D.commit_chunked_upload t (!upload_id) fn
    >>= fun m -> Lwt_unix.close fd
    >>= fun () -> Lwt_io.printlf "Sended: %s\nMetadata:\n%s"
                  fn (Dropbox_j.string_of_metadata m)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
