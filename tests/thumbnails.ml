open Lwt
module D = Dropbox_lwt_unix

(** If there is one entry, we call thumbnails with the path as Sys.argv.(0).
    If there is two entries, we call thumbnails with the path as
       Sys.argv.(0) and the param format as the second.
    If there is three entries, we add the param size as the third argument *)

let download t ?(size="s") ?format fn =
  let y = String.index fn '.' in
  let extension = String.sub fn (y + 1) (String.length fn - y - 1) in
  (** Si le format que l'on a envoyé est égale à l'extension de l'image alors
      ok. Si elle est différente, pour éviter les conflits, on renvoie
      l'extension de l'image. Enfin, rien n'était spécifié dans format (un
      seul argument, alors on met "" par défaut (les mauvaises extensions
      sont gérées par 404. *)
  let format = match format with
    | Some f -> if extension = f then f
                else extension
    | None -> "" in (** .jpg files does work *)
  D.thumbnails t ~size ~format fn >>= function
    | None -> Lwt_io.printlf "No image named %S." fn
    | Some(m, stream) ->
       (* Save stream to the disk *)
       let fname = Filename.basename ("thumbnails_of_" ^ fn) in
       Lwt_unix.(openfile fname [O_WRONLY; O_CREAT; O_TRUNC] 0o664)
       >>= fun fd ->
       let write s =
         Lwt_unix.write fd s 0 (String.length s) >>= fun _ ->
         return_unit in
       Lwt_stream.iter_s write stream >>= fun () ->
       Lwt_unix.close fd >>= fun () ->
       Lwt_io.printlf "Wrote a thumbnails of %S (%s, %s), %s"
                      fn m.D.size m.D.mime_type
                      (Dropbox.Date.to_string m.D.modified)

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | a -> if List.length a = 1 then
           download t (List.hd a)
	 else if List.length a = 2 then
           match List.nth a 1, List.nth a 0 with
           | fn, format -> download t ~format fn
         else if List.length a = 3 then
           match List.nth a 2, List.nth a 1, List.nth a 0 with
           | fn, format, size -> download t ~format ~size fn
	 else
           Lwt_io.printlf "Thumbnails function take at least 3 arguments."

let () =
  Common.run main
