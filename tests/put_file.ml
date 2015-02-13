open Lwt
module D = Dropbox_lwt_unix

let buffer = "test" ;;
let stream, push = Lwt_stream.create () ;;
push (Some buffer) ;;

let upload t fn =
  let stream = 
    let in_channel = open_in fn in
    Lwt_stream.from_direct 
    (fun _ -> try Some (input_line in_channel) with
              End_of_file -> None) in
  D.put_file t fn (String.length buffer) stream >>= fun metadata ->
  Lwt_io.printlf "Sended %s" fn

let main t args =
  match args with
  | [] -> upload t "test.txt"
  | _ -> Lwt_list.iter_p (upload t) args

let () =
  Common.run main
