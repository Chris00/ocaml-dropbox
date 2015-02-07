open Lwt
module D = Dropbox_lwt_unix

let buffer = "test" ;;
let stream, push = Lwt_stream.create () ;;
push (Some buffer) ;;


let upload t fn =
  D.put_file t fn 4 stream

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | _ ->
  (* You can replace Lwt_list.iter_p by Lwt_list.iter_s for a
  sequential upload and see how much slower it is. *)
    Lwt_list.iter_p (upload t) args

let () =
Common.run main
