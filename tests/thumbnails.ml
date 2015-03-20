open Lwt
module D = Dropbox_lwt_unix

(** If there is one entry, we call thumbnails with the path as Sys.argv.(0).
    If there is two entries, we call thumbnails with the path as
       Sys.argv.(0) and the param size as the second.
    If there is three entries, we add the param format as the third argument *)

let string_to_size = function
  | "xs" -> `Xs
  | "s" -> `S
  | "m" -> `M
  | "l" -> `L
  | "xl" -> `Xl
  | _ -> invalid_arg "Size must be xs, s, m, l or xl."

let string_to_format = function
  | "jpeg" -> `Jpeg
  | "jpg" -> `Jpeg
  | "png" -> `Png
  | _ -> invalid_arg "Format must be jpeg or png."


let download t ?format ?(size="s") fn =
  let y = String.index fn '.' in
  let extension = String.sub fn (y + 1) (String.length fn - y - 1) in
  let send_thumbnails = match format with
  (** if the format of .png image is not specified, it will return a wrong
      thumbnail *)
    | None -> D.thumbnails t ~size:(string_to_size size)
                           ~format:(string_to_format extension)  fn
    | Some f -> D.thumbnails t ~size:(string_to_size size)
                             ~format:(string_to_format f) fn in
  send_thumbnails >>= function
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
  Lwt_io.printlf "Wrote a thumbnails of %S" fn

let main t args =
  match args with
  | [] -> Lwt_io.printlf "No file specified."
  | a -> if List.length a = 1 then
           download t (List.hd a)
	 else if List.length a = 2 then
           match List.nth a 1, List.nth a 0 with
           | fn, size -> download t ~size fn
         else if List.length a = 3 then
           match List.nth a 2, List.nth a 1, List.nth a 0 with
           | fn, size, format -> download t ~format
                                 ~size fn
	 else
           Lwt_io.printlf "Thumbnails function take a maximum of 3 arguments."

let () =
  Common.run main
