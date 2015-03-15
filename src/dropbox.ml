(* https://www.dropbox.com/developers/core/docs *)

open Printf
open Lwt
module Json = Dropbox_j
module Date = Dropbox_date

(* Error handling
 ***********************************************************************)

type error_description = Json.error_description
                       = { error: string;
                           error_description: string }

type error =
  | Invalid_arg of error_description
  | Invalid_token of error_description
  | Invalid_oauth of error_description
  | Too_many_requests of error_description
  | Try_later of int option * error_description
  | Quota_exceeded of error_description
  | Server_error of int * error_description
  | Conflict of error_description
  | Length_required of error_description
  | Not_found404 of error_description

(* FIXME: Do we want to render the values as strings closer to OCaml? *)
let string_of_error = function
  | Invalid_arg e ->
     "Invalid_arg " ^ Json.string_of_error_description e
  | Invalid_token e ->
     "Invalid_token " ^ Json.string_of_error_description e
  | Invalid_oauth e ->
     "Invalid_oauth " ^ Json.string_of_error_description e
  | Too_many_requests e ->
     "Too_many_requests " ^ Json.string_of_error_description e
  | Try_later (sec, e) ->
     (match sec with
      | None -> "Try_later " ^ Json.string_of_error_description e
      | Some s ->
         sprintf "Try_later(%i, %s)" s (Json.string_of_error_description e))
  | Quota_exceeded e ->
     "Quota_exceeded " ^ Json.string_of_error_description e
  | Server_error (st, e) ->
     sprintf "Server_error(%i, %s)" st (Json.string_of_error_description e)
  | Conflict e -> 
     "Conflict " ^ Json.string_of_error_description e
  | Length_required e ->
     "Length_required " ^ Json.string_of_error_description e
  | Not_found404 e ->
     "Not_found404 " ^ Json.string_of_error_description e

exception Error of error

let () =
  Printexc.register_printer (function Error e -> Some(string_of_error e)
                                    | exn -> None)


let fail_error body f =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let e = Json.error_description_of_string body in
  fail(Error(f e))

let check_errors_k k ((rq, body) as r) =
  match rq.Cohttp.Response.status with
  | `Bad_request -> fail_error body (fun e -> Invalid_arg e)
  | `Unauthorized -> fail_error body (fun e -> Invalid_token e)
  | `Forbidden -> fail_error body (fun e -> Invalid_oauth e)
  | `Too_many_requests -> fail_error body (fun e -> Too_many_requests e)
  | `Service_unavailable ->
     (match Cohttp.(Header.get rq.Response.headers "retry-after") with
      | None -> fail_error body (fun e -> Try_later(None, e))
      | Some retry ->
         try
           (* FIXME: [retry] can also be a date *)
           let s = int_of_string retry in
           fail_error body (fun e -> Try_later(Some s, e))
         with _ ->
           fail_error body (fun e -> Try_later(None, e)) )
  | `Insufficient_storage -> fail_error body (fun e -> Quota_exceeded e)
  | `Conflict -> fail_error body (fun e -> Conflict e)
  | `Length_required -> fail_error body (fun e -> Length_required e)
  | `Not_found -> fail_error body (fun e -> Not_found404 e)
  | _ -> k r

let check_errors =
  let std_err ((rq, body) as r) =
    let st = Cohttp.Code.code_of_status rq.Cohttp.Response.status  in
    if Cohttp.Code.is_error st then
      fail_error body (fun e -> Server_error(st, e))
    else
       return(r) in
  check_errors_k std_err

let check_errors_404 f =
  let std_err ((rq, body) as r) =
    if rq.Cohttp.Response.status = `Not_found then
      return None
    else
      let st = Cohttp.Code.code_of_status rq.Cohttp.Response.status in
      if Cohttp.Code.is_error st then
        fail_error body (fun e -> Server_error(st, e))
      else
        f r in
  check_errors_k std_err


(* Signature & Functor
 ***********************************************************************)

module type S = sig

  module OAuth2 : sig
    val authorize : ?state: string ->
                    ?force_reapprove: bool ->
                    ?disable_signup: bool ->
                    id: string ->
                    [`Token of Uri.t | `Code of Uri.t option] -> Uri.t

    type code = string
    val code_of_uri : Uri.t -> (code * string) option

    type token = string

    val token_of_uri : Uri.t -> (token * string) option

    val token : ?redirect_uri: Uri.t ->
                code -> id: string -> secret: string -> token Lwt.t
  end

  type t
  val session : OAuth2.token -> t
  val token : t -> OAuth2.token

  type name_details = Dropbox_t.name_details
                    = { familiar_name: string;
                        given_name: string;
                        surname: string }

  type team = Dropbox_t.team
            = { name: string;
                team_id: int }

  type quota_info = Dropbox_t.quota_info
                  = { shared: int;
                      quota: int;
                      normal: int }

  type info = Dropbox_t.info
            = { uid: int;
                display_name: string;
                email_verified: bool;
                name_details: name_details;
                referral_link: Uri.t;
                country: string;
                locale: string;
                is_paired: bool;
                team: team option;
                quota_info: quota_info }

  val info : ?locale: string -> t -> info Lwt.t

  type metadata = Dropbox_t.metadata = {
      size: string;
      bytes: int;
      mime_type: string;
      path: string;
      is_dir: bool;
      is_deleted: bool;
      rev: string;
      hash: string;
      thumb_exists: bool;
      icon: string;
      modified: Date.t;
      client_mtime: Date.t;
      root: [ `Dropbox | `App_folder ]
    }

  type chunked_upload = Dropbox_t.chunked_upload
                      = { upload_id: string;
                          offset: int;
                          expires: Date.t }

  val get_file : t -> ?rev: string -> ?start: int -> ?len: int ->
                 string -> (metadata * string Lwt_stream.t) option Lwt.t

  val stream_files_put : t -> ?locale: string -> ?overwrite: bool ->
                         ?parent_rev: string -> ?autorename: bool -> string ->
                         int -> string Lwt_stream.t -> metadata Lwt.t

  val cohttp_body_files_put : t -> ?locale: string -> ?overwrite: bool ->
                              ?parent_rev: string -> ?autorename: bool ->
                              string -> int -> Cohttp_lwt_body.t ->
                              metadata Lwt.t

  val chunked_upload : t -> ?upload_id: string -> ?offset: int ->
                       Cohttp_lwt_body.t -> chunked_upload Lwt.t

  val commit_chunked_upload : t -> ?locale: string -> ?overwrite: bool ->
                              ?parent_rev: string -> ?autorename: bool ->
                              ?upload_id: string -> string ->
                              metadata Lwt.t
end

module Make(Client: Cohttp_lwt.Client) = struct

  module OAuth2 = struct

    let authorize_uri =
      Uri.of_string "https://www.dropbox.com/1/oauth2/authorize"

    let authorize ?(state="") ?(force_reapprove=false) ?(disable_signup=false)
                  ~id:client_id response =
      let q = ["client_id", [client_id];
               "state", [state];
               "force_reapprove", [string_of_bool force_reapprove];
               "disable_signup", [string_of_bool disable_signup] ] in
      let q = match response with
        | `Token uri -> ("response_type", ["token"])
                       :: ("redirect_uri", [Uri.to_string uri]) :: q
        | `Code(Some uri) -> ("response_type", ["code"])
                            :: ("redirect_uri", [Uri.to_string uri]) :: q
        | `Code None -> ("response_type", ["code"]) :: q in
      Uri.with_query authorize_uri q

    type code = string

    let code_of_uri u =
      match Uri.get_query_param u "code" with
      | Some code ->
         let state = match Uri.get_query_param u "state" with
           | Some s -> s
           | None -> "" in
         Some(code, state)
      | None -> None

    type token = string

    let get_query_param name q = String.concat "" (List.assoc name q)

    let token_of_uri u =
      match Uri.fragment u with
      | None -> None
      | Some f ->
         let q = Uri.query_of_encoded f in
         try
           let state = try get_query_param "state" q
                       with _ -> "" in
           Some (get_query_param "access_token" q,  state)
         with Not_found -> None

    let token_uri = Uri.of_string "https://api.dropbox.com/1/oauth2/token"

    let token ?redirect_uri code ~id ~secret =
      (* FIXME: do we want to allow the possibility to use HTTP basic
       authentication (and why/why not)? *)
      let q = ["code", [code];
               "grant_type", ["authorization_code"];
               "client_id", [id];
               "client_secret", [secret] ] in
      let q = match redirect_uri with
        | None -> q
        | Some u -> ("redirect_uri", [Uri.to_string u]) :: q in
      Client.post (Uri.with_query token_uri q) >>=
      check_errors >>= fun (_, body) ->
      Cohttp_lwt_body.to_string body >>= fun body ->
      return((Json.token_of_string body).Json.access_token)
  end

  include Dropbox_t

  type t = OAuth2.token

  let token t = t

  let session token = token

  let headers t =
    let bearer = "Bearer " ^ (token t) in
    Cohttp.Header.init_with "Authorization" bearer

  let info_uri = Uri.of_string "https://api.dropbox.com/1/account/info"

  let info ?locale t =
    let u = match locale with
      | None -> info_uri
      | Some l -> Uri.with_query info_uri ["locale", [l]] in
    Client.get ~headers:(headers t) u >>= check_errors >>= fun (_, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    return(Json.info_of_string body)


  let get_metadata k (r, body) =
    (* Extract content metadata from the header *)
    match Cohttp.(Header.get r.Response.headers "x-dropbox-metadata") with
    | Some h ->
       let metadata = Json.metadata_of_string h in
       k metadata body
    | None ->
       (* Should not happen *)
       let msg = {
           error = "x-dropbox-metadata";
           error_description = "Missing x-dropbox-metadata header" } in
       fail(Error(Server_error(500, msg)))

  let stream_of_file =
    get_metadata (fun metadata body ->
                  return(Some(metadata, Cohttp_lwt_body.to_stream body)))

  let empty_stream =
    get_metadata (fun metadata body ->
                  Cohttp_lwt_body.drain_body body >>= fun () ->
                  return(Some(metadata, Lwt_stream.of_list [])))

  let get_file t ?rev ?start ?len fn =
    let headers = headers t in
    let headers, must_download = match start, len with
      | Some s, Some l ->
         let s = if s < 0 then 0 else s in
         if l <= 0 then (Cohttp.Header.add headers "Range" ("bytes=0-0"), false)
         else
           let range = string_of_int s ^ "-" ^ string_of_int(s + l - 1) in
           (Cohttp.Header.add headers "Range" ("bytes=" ^ range),
            true)
      | Some s, None ->
         let range = string_of_int s ^ "-" in
         (Cohttp.Header.add headers "Range" ("bytes=" ^ range),
          true)
      | None, Some l ->
         if l <= 0 then (Cohttp.Header.add headers "Range" ("bytes=0-0"), false)
         else
           (Cohttp.Header.add headers "Range" ("bytes=-" ^ string_of_int l),
            true)
      | None, None -> headers, true in
    let u =
      Uri.of_string("https://api-content.dropbox.com/1/files/auto/" ^ fn) in
    let u = match rev with None -> u
                         | Some r -> Uri.with_query u ["rev", [r]] in
    Client.get ~headers u
    >>= check_errors_404 (if must_download then stream_of_file
                          else empty_stream)


  let stream_files_put t ?locale ?(overwrite = true) ?parent_rev 
                       ?(autorename = true) fn len stream =
  (* let headers = headers t in *)
    let headers = Cohttp.Header.add (headers t)
      "Content-Length" (string_of_int (len)) in
    let u =
      Uri.of_string("https://api-content.dropbox.com/" ^
                      "1/files_put/auto/" ^ fn) in
    let param = ("overwrite", [string_of_bool overwrite]) ::
      ("autorename", [string_of_bool autorename]) :: [] in
    let param = match locale with
      | Some l -> ("locale", [l]) :: param
      | None -> param in
    let param = match parent_rev with
      | Some p_rev -> ("parent_rev",[p_rev]) :: param
      | None -> param in
    let u = Uri.with_query u param in
    Client.put ~headers ~body:(Cohttp_lwt_body.of_stream stream) u >>=
    check_errors >>= fun (_, body) -> Cohttp_lwt_body.to_string body
    >>= fun body -> return(Json.metadata_of_string body)


  let cohttp_body_files_put t ?locale ?(overwrite = true) ?parent_rev 
                            ?(autorename = true) fn len stream =
    (* let headers = headers t in  *)
    let headers = Cohttp.Header.add (headers t)
      "Content-Length" (string_of_int (len)) in
    let u =
      Uri.of_string("https://api-content.dropbox.com/" ^
                      "1/files_put/auto/" ^ fn) in
    let param = ("overwrite", [string_of_bool overwrite]) ::
      ("autorename", [string_of_bool autorename]) :: [] in
    let param = match locale with
      | Some l -> ("locale", [l]) :: param
      | None -> param in
    let param = match parent_rev with
      | Some p_rev -> ("parent_rev",[p_rev]) :: param
      | None -> param in
    let u = Uri.with_query u param in
    Client.put ~headers ~body:stream u >>=
    check_errors >>= fun (_, body) -> Cohttp_lwt_body.to_string body
    >>= fun body -> return(Json.metadata_of_string body)


  let chunked_upload t ?upload_id ?offset chunked_data =
    let u =
      Uri.of_string("https://api-content.dropbox.com/1/chunked_upload") in
    let param = match upload_id with
      | Some id -> ("upload_id",[id]) :: []
      | None -> [] in
    let param = match offset with
      | Some offset -> ("offset",[string_of_int offset]) :: param
      | None -> param in
    let u = Uri.with_query u param in
    Client.put ~body:chunked_data ~chunked:true ~headers:(headers t)  u >>=
    check_errors >>= fun (_, body) -> Cohttp_lwt_body.to_string body
    >>= fun body -> return(Json.chunked_upload_of_string body)


  let commit_chunked_upload t ?locale ?(overwrite=true) ?parent_rev
                            ?(autorename=true) ?upload_id fn =
    let u = Uri.of_string("https://api-content.dropbox.com/1/"
            ^ "commit_chunked_upload/auto/" ^ fn) in
    let param = ("overwrite",[string_of_bool overwrite]) ::
      ("autorename",[string_of_bool autorename]) :: [] in
    let param = match locale with
      | Some l -> ("locale",[l]) :: param
      | None -> param in
    let param = match upload_id with
      | Some id -> ("upload_id",[id]) :: param
      | None -> param in
    let u = Uri.with_query u param in 
    Client.post ~headers:(headers t) u >>=
    check_errors >>= fun (_, body) -> Cohttp_lwt_body.to_string body
    >>= fun body -> return(Json.metadata_of_string body)
end
