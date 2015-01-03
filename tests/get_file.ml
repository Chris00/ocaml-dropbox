open Lwt
module D = Dropbox_lwt_unix

let download t fn =
  D.get_file t fn >>= function
  | None -> Lwt_io.printlf "No file named %S." fn
  | Some(m, stream) ->
     (* Save stream to the disk *)
     let fname = Filename.basename fn in
     Lwt_unix.(openfile fname [O_WRONLY; O_CREAT; O_TRUNC] 0o664)
     >>= fun fd ->
     let write s =
       Lwt_unix.write fd s 0 (String.length s) >>= fun _ ->
       return_unit in
     Lwt_stream.iter_s write stream >>= fun () ->
     Lwt_unix.close fd >>= fun () ->
     Lwt_io.printlf "Wrote %S (%s, %s), rev: %s"
                    fn m.D.size m.D.mime_type m.D.rev

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | _ ->
     (* You can replace Lwt_list.iter_p by Lwt_list.iter_s for a
        sequential download and see how much slower it is. *)
     Lwt_list.iter_p (download t) args

let () =
  Common.run main
