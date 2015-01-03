open Lwt
module D = Dropbox_lwt_unix

let download t fn =
  D.get_file t fn >>= function
  | None -> Lwt_io.printlf "No file named %S." fn
  | Some(m, stream) ->
     (* Save stream to the disk *)
     let fname = Filename.basename fn in
     let fd0 = Unix.openfile fname [Unix.O_WRONLY; Unix.O_CREAT] 0o664 in
     let fd = Lwt_unix.of_unix_file_descr fd0 in
     let write s =
       Lwt_unix.write fd s 0 (String.length s) >>= fun _ ->
       return_unit in
     Lwt_stream.iter_s write stream >>= fun _ ->
     Unix.close fd0;
     return_unit

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | _ -> Lwt_list.iter_p (download t) args

let () =
  Common.run main
