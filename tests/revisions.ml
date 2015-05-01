open Lwt
module D = Dropbox_lwt_unix

let revisions t fn =
  D.revisions t fn >>= function
  | Some metadata_list ->
    let get_rev metadata = metadata.D.rev in
    let revisions = String.concat ", rev: " (List.map get_rev metadata_list) in
    Lwt_io.printlf "rev: %s" revisions
  | None -> Lwt_io.printlf "No file %s" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file or folder specified"
  | _ -> Lwt_list.iter_p (revisions t) args

let () =
  Common.run main
