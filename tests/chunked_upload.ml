open Lwt
module D = Dropbox_lwt_unix

let create_list i =
  let list = ref [] in
  for j = 0 to i do
    list := abs(j - i) :: !list;
  done;
  !list ;;

let upload t fn =
  Lwt_unix.(openfile fn [O_RDONLY] 0) >>= fun fd ->
  Lwt_unix.stat fn >>= fun u -> return(u.Lwt_unix.st_size) >>= fun size ->
  let lgth_buff = 4194304 in
  (** If the size of the file is < 4Mo, we use file_put function, otherwise
      chunked_upload function *)
  if size < lgth_buff then
    let buffer = String.create size in
    Lwt_unix.read fd buffer 0 size >>= fun _ ->
    let stream = Cohttp_lwt_body.of_string buffer in
    D.cohttp_body_files_put t fn size stream
    >>= fun m -> Lwt_unix.close fd >>= fun () ->
    Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
  else
    let numb = size / lgth_buff in
    let buffer = String.create lgth_buff in
    let upload_id = ref "" in
    let read s = match s with
      | 0 -> Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
             let stream = Cohttp_lwt_body.of_string buffer in
             D.chunked_upload t stream >>= fun chunked_upload ->
             upload_id := (chunked_upload.D.upload_id);
             Lwt_io.printlf "\nUpload_id: %s" (!upload_id)
             >>= fun () -> return ()
      | num -> if num = numb then
                 Lwt_io.printlf "Offset: %i \nEnd of file"
                   (num * lgth_buff) >>=
                 fun () -> Lwt_unix.read fd buffer 0 lgth_buff >>= fun size ->
                 let stream = Cohttp_lwt_body.of_string
                   (String.sub buffer 0 size) in
                 D.chunked_upload t ~upload_id:(!upload_id)
                   ~offset:(num * lgth_buff) stream  >>=
                 fun chunked_upload -> return ()
               else
                 Lwt_io.printlf "Offset: %i" (num * lgth_buff) >>=
                 fun () -> Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
                 let stream = Cohttp_lwt_body.of_string buffer in
                 D.chunked_upload t ~upload_id:(!upload_id)
                   ~offset:(num * lgth_buff) stream >>= fun _ -> return()
    in
    Lwt_list.iter_s read (create_list numb) >>= fun () ->
    D.commit_chunked_upload t ~upload_id:(!upload_id) fn
    >>= fun m -> Lwt_unix.close fd >>= fun () ->
    Lwt_io.printlf "Sended: %s\nMetadata:\n%s"
      fn (Dropbox_j.string_of_metadata m)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
