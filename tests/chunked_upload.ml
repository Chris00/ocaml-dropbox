open Lwt
module D = Dropbox_lwt_unix

exception Wrong_offset of string

let upload t fn =
  (** We first open the file and get its size *)
  Lwt_unix.(openfile fn [O_RDONLY] 0)
  >>= fun fd -> Lwt_unix.stat fn >>= fun u ->
  let size = u.Lwt_unix.st_size in
  let lgth_buff = 4194304 in
  (** If the size of the file is < 4Mo, we use cohttp_body_files_put
      function. Otherwise chunked_upload function *)
  if size < lgth_buff then
    let buffer = Bytes.create size in
    Lwt_unix.read fd buffer 0 size >>= fun _ ->
    let stream = Cohttp_lwt_body.of_string (Bytes.to_string buffer) in
    D.cohttp_body_files_put t fn size stream
    >>= fun metadata -> Lwt_unix.close fd
    >>= fun () -> Lwt_io.printlf "Send: %s\nMetadata:\n%s"
                  fn (Dropbox_j.string_of_metadata metadata)
  else (** The size is > 4Mo *)
    let numb = size / lgth_buff in
    let buffer = Bytes.create lgth_buff in

    let rec send_chunks ?(upload_id="") ?(offset=0) lgth =
      (** To see if the offset = the number of bytes actually read
          by the function. If not, raise exception Wrong_offset *)
      if offset = ((numb - lgth) * lgth_buff) then
        (** Initializing with upload_id *)
        if lgth = numb then
          Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
          let stream = Cohttp_lwt_body.of_string (Bytes.to_string buffer) in
          D.chunked_upload t stream >>= fun chkd_upld ->
          Lwt_io.printlf "Upload_id: %s" (chkd_upld.D.upload_id)
          >>= fun () -> send_chunks ~upload_id:(chkd_upld.D.upload_id)
                        ~offset:(chkd_upld.D.offset) (lgth-1)
        (** The last time we enter in send_chunks *)
        else if lgth = 0 then
          Lwt_io.printlf "Offset: %i \nEnd of file" offset
          >>= fun () -> Lwt_unix.read fd buffer 0 lgth_buff >>= fun size ->
          let stream = Cohttp_lwt_body.of_string
                       (Bytes.to_string (Bytes.sub buffer 0 size)) in
          D.chunked_upload t ~upload_id ~offset stream
          >>= fun _ -> return(upload_id)
        else
          Lwt_io.printlf "Offset: %i" offset
          >>= fun () -> Lwt_unix.read fd buffer 0 lgth_buff >>= fun _ ->
          let stream = Cohttp_lwt_body.of_string (Bytes.to_string buffer) in
          D.chunked_upload t ~upload_id ~offset stream
          >>= fun chkd_upld -> send_chunks ~upload_id
                               ~offset:(chkd_upld.D.offset) (lgth-1)
      else
        let msg = "Last offset: " ^ (string_of_int offset) in
        fail(Wrong_offset msg)
    in

    (** Finally, we finish the upload of the file by using
        commit_chunked_upload *)
    send_chunks numb >>= fun upload_id ->
    D.commit_chunked_upload t upload_id fn
    >>= fun metadata -> Lwt_unix.close fd
    >>= fun () -> Lwt_io.printlf "Send: %s\nMetadata:\n%s"
                  fn (Dropbox_j.string_of_metadata metadata)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
