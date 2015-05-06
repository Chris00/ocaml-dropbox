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
  | Not_modified of error_description
  | Not_acceptable of error_description

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
  | Not_modified e ->
     "Not_modified " ^ Json.string_of_error_description e
  | Not_acceptable e ->
     "Not_acceptable " ^ Json.string_of_error_description e

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
  | `Not_modified -> fail_error body (fun e -> Not_modified e)
  | `Not_acceptable -> fail_error body (fun e -> Not_acceptable e)
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

  type photo_info = Dropbox_t.photo_info
                  = { time_taken: Date.t option;
                      lat_long: (float * float) option }

  type video_info = Dropbox_t.video_info
                  = { time_taken: Date.t option;
                      duration: float option;
                      lat_long: (float * float) option }

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
      photo_info: photo_info option;
      video_info: video_info option;
      icon: string;
      modified: Date.t option;
      client_mtime: Date.t option;
      root: [ `Dropbox | `App_folder ];
      contents: metadata list;
    }

  type cursor

  (* It is better that [delta] is not shared between various
     instantiations of the functor so it is good it does not appear as
     an alias. *)
  type delta
    = { entries: (string * metadata option) list;
        reset: bool;
        cursor: cursor;
        has_more: bool }

  type longpoll_delta
    = Dropbox_t.longpoll_delta
    = { changes: bool;
        backoff: int option }

  val get_file : t -> ?rev: string -> ?start: int -> ?len: int ->
                 string -> (metadata * string Lwt_stream.t) option Lwt.t

  val metadata : t -> ?file_limit: int -> ?hash: string -> ?list: bool ->
                 ?include_deleted: bool -> ?rev: string -> ?locale: string ->
                 ?include_media_info: bool -> ?include_membership: bool ->
                 string -> metadata option Lwt.t

  val delta : ?cursor: cursor -> ?locale: string -> ?path_prefix: string
              -> ?include_media_info: bool -> t -> delta Lwt.t

  val latest_cursor : ?path_prefix: string -> ?include_media_info: bool
                      -> t -> cursor Lwt.t

  val longpoll_delta : t -> ?timeout: int -> cursor -> longpoll_delta Lwt.t

  val revisions : t -> ?rev_limit: int -> ?locale: string -> string ->
                  metadata list option Lwt.t

  val restore : t -> ?locale: string -> rev:string -> string ->
                metadata option Lwt.t

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

  let metadata_of_response (_, body) =
    Cohttp_lwt_body.to_string body
    >>= fun body -> return(Some(Json.metadata_of_string body))

  let metadata t ?(file_limit=10_000) ?(hash="") ?(list=true)
               ?(include_deleted=false) ?(rev="") ?(locale="")
               ?(include_media_info=false) ?include_membership fn =
    let u = Uri.of_string("https://api.dropbox.com/1/metadata/auto/" ^ fn) in
    let file_limit = if file_limit < 0 then 0 else file_limit in
    let q = [("list", [string_of_bool list]);
             ("file_limit", [string_of_int file_limit]);
             ("include_deleted", [string_of_bool include_deleted]);
             ("include_media_info", [string_of_bool include_media_info]);
            ] in
    let q = if hash <> "" then ("hash", [hash]) :: q else q in
    let q = if locale <> "" then ("locale", [locale]) :: q else q in
    let q = if rev <> "" then ("rev", [rev]) :: q else q in
    let u = Uri.with_query u q in
    Client.get ~headers:(headers t) u
    >>= check_errors_404 metadata_of_response


  type cursor = { cursor: string;
                  path_prefix: string;
                  include_media_info: bool }

  type delta = { entries: (string * Json.metadata option) list;
                 reset: bool;
                 cursor: cursor;
                 has_more: bool }

  (* FIXME: [path_concat] should probably be in Uri. *)
  let path_concat p1 p2 =
    if p2 = "" then p1
    else if p1 = "" then p2
    else if p1.[String.length p1 - 1] = '/' then p1 ^ p2
    else p1 ^ "/" ^ p2

  let delta_uri = Uri.of_string "https://api.dropbox.com/1/delta"

  let delta ?cursor ?(locale="") ?(path_prefix="")
            ?(include_media_info=false) t =
    let param = match cursor with
      | Some (c: cursor) ->
         let param = [("cursor", [c.cursor])] in
         let param =
           if c.path_prefix <> "" then
             ("path_prefix", [path_concat c.path_prefix path_prefix]) :: param
           else param in
         if c.include_media_info then
           ("include_media_info",[string_of_bool c.include_media_info])
           :: param
         else param
      | None ->
         let param =
           [("include_media_info", [string_of_bool include_media_info])] in
         if path_prefix <> "" then ("path_prefix", [path_prefix]) :: param
         else param in
    let param = if locale <> "" then ("locale", [locale]) :: param
                else param in
    let u = Uri.with_query delta_uri param in
    Client.post ~headers:(headers t) u
    >>= check_errors >>= fun (_, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    let delta = Json.delta_json_of_string body in
    let cursor = { cursor = delta.Json.cursor;
                   path_prefix;
                   include_media_info } in
    return({ entries = delta.Json.entries;
             reset = delta.Json.reset;
             cursor;
             has_more = delta.Json.has_more })


  let latest_cursor_uri =
    Uri.of_string "https://api.dropbox.com/1/delta/latest_cursor"

  let latest_cursor ?(path_prefix="") ?(include_media_info=false) t =
    let param = [("include_media_info",[string_of_bool include_media_info])] in
    let param =
      if path_prefix <> "" then ("path_prefix", [path_prefix]) :: param
      else param in
    let u = Uri.with_query latest_cursor_uri param in
    Client.post ~headers:(headers t) u
    >>= check_errors >>= fun(_, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    let c = Json.latest_cursor_of_string body in
    return({ cursor = c.Json.latest_cursor;
             path_prefix;
             include_media_info })


  let longpoll_delta_uri =
    Uri.of_string "https://api-notify.dropbox.com/1/longpoll_delta"

  let longpoll_delta t ?(timeout=30) (c: cursor) =
    let timeout = if timeout < 30 then 30
                  else if timeout > 480 then 480
                  else timeout in
    let param = [("timeout", [string_of_int timeout]);
                 ("cursor", [c.cursor])] in
    let u = Uri.with_query longpoll_delta_uri param in
    Client.get ~headers:(headers t) u >>= check_errors
    >>= fun(_, body) -> Cohttp_lwt_body.to_string body
    >>= fun body -> return(Json.longpoll_delta_of_string body)

  let metadata_list_of_response (_, body) =
    Cohttp_lwt_body.to_string body
    >>= fun body -> return(Some(Json.metadata_list_of_string body))

  let revisions t ?(rev_limit=10) ?(locale="") fn =
    let u = Uri.of_string("https://api.dropbox.com/1/revisions/auto/" ^ fn) in
    let rev_limit = if rev_limit < 0 then 0
                    else if rev_limit > 1000 then 1000
                    else rev_limit in
    let q = [("rev_limit",[string_of_int rev_limit])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let u = Uri.with_query u q in
    Client.get ~headers:(headers t) u
    >>= check_errors_404 metadata_list_of_response

 let restore t ?(locale="") ~rev fn =
    let u = Uri.of_string("https://api.dropbox.com/1/restore/auto/" ^ fn) in
    let q = [("rev",[rev])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let u = Uri.with_query u q in
    Client.post ~headers:(headers t) u
    >>= check_errors_404 metadata_of_response
end
