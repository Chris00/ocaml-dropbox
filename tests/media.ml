open Lwt
module D = Dropbox_lwt_unix

let media t fn =
  D.media t fn >>= function
  | Some shared_url -> Lwt_io.printlf "%s"
                       (Dropbox_j.string_of_shared_url shared_url)
  | None -> Lwt_io.printlf "No file %s" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (media t) args

let () =
  Common.run main
