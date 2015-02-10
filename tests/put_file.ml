open Lwt
module D = Dropbox_lwt_unix

let buffer = "test" ;;
let stream, push = Lwt_stream.create () ;;
push (Some buffer) ;;

let upload t fn =
  D.put_file t fn (String.length buffer) stream >>= fun metadata ->
  Lwt_io.printlf "Sended %s" fn

let main t args =
  match args with
  | [] -> upload t "test.txt"
  | _ -> Lwt_io.printlf "Don't write something after .native"

let () =
  Common.run main
