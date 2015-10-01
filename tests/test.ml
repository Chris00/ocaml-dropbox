open Lwt
module D = Dropbox_lwt_unix

(** The function create an folder called "test-ocaml-dropbox"
    and create [n] different files within it.
    To delete the folder, just use the function {!delete} with
    "test-ocaml-dropbox" as argument. **)

let create_list length =
  let rec incr l acc =
    if l < 0 then acc
    else incr (l-1) ((string_of_int l)::acc) in
  incr length []

let create_files t length=
  D.create_folder t "test-ocaml-dropbox"
  >>= function
  | `Invalid s -> Lwt_io.printlf "Invalid: %s" s
  | `Some m -> Lwt_io.printlf "%s" (Dropbox_j.string_of_metadata m)
  >>= fun () ->
  let put_files path =
    D.files_put t ("test-ocaml-dropbox/" ^ path ^ ".txt") (`String path)
    >>= fun m -> Lwt_io.printlf "%s" (m.D.path)
  in
  Lwt_list.iter_p put_files (create_list length)
  >>= fun () -> Lwt_io.printlf "Wrote %i files in test-ocaml-dropbox/" length

let main t args =
  match args with
  | [length] -> create_files t (int_of_string length)
  | _ -> Lwt_io.printlf "%s <number of files to be created>\n" Sys.argv.(0)

let () =
  Common.run main
