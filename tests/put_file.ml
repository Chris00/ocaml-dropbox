open Lwt
module D = Dropbox_lwt_unix

let buffer = "test" ;;
let stream = Cohttp_lwt_body.of_string buffer;;

let upload t fn =
  D.put_file t fn (String.length buffer) stream >>= function
  | None -> Lwt_io.printlf "Error"
  | Some (m, stream) -> Lwt_io.printlf "Send %S" fn

let main t args =
  match args with
  | [] -> upload t "test.txt"
  | _ -> Lwt_io.printlf "Don't write something after .native"

let () =
  Common.run main
