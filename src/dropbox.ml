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
    val authorize : ?response_type: [`Token | `Code] ->
                    ?redirect_uri: Uri.t ->
                    ?state: string ->
                    ?force_reapprove: bool ->
                    ?disable_signup: bool ->
                    string -> Uri.t

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

  type metadata = {
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

  val get_file : t -> ?rev: string -> ?start: int -> ?len: int ->
                 string -> (metadata * string Lwt_stream.t) option Lwt.t

end

module Make(Client: Cohttp_lwt.Client) = struct

  module OAuth2 = struct

    let authorize_uri =
      Uri.of_string "https://www.dropbox.com/1/oauth2/authorize"

    let authorize ?(response_type=`Code) ?redirect_uri ?(state="")
                  ?(force_reapprove=false) ?(disable_signup=false) client_id =
      let response_type = match response_type with
        | `Token -> "token"
        | `Code -> "code" in
      let q = ["response_type", [response_type];
               "client_id", [client_id];
               "state", [state];
               "force_reapprove", [string_of_bool force_reapprove];
               "disable_signup", [string_of_bool disable_signup] ] in
      let q = match redirect_uri with
        | Some u -> ("redirect_uri", [Uri.to_string u]) :: q
        | None -> q in
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


  let stream_of_file (r, body) =
    (* Extract content metadata from the header *)
    match Cohttp.(Header.get r.Response.headers "x-dropbox-metadata") with
    | Some h ->
       let metadata = Json.metadata_of_string h in
       return(Some(metadata, Cohttp_lwt_body.to_stream body))
    | None ->
       (* Should not happen *)
       let msg = {
           error = "x-dropbox-metadata";
           error_description = "Missing x-dropbox-metadata header" } in
       fail(Error(Server_error(500, msg)))

  let get_file t ?rev ?start ?len fn =
    let headers = headers t in
    let headers = match start, len with
      | Some s, Some l ->
         let range = string_of_int s ^ "-" ^ string_of_int(s + l - 1) in
         Cohttp.Header.add headers "Range" ("bytes=" ^ range)
      | Some s, None ->
         let range = string_of_int s ^ "-" in
         Cohttp.Header.add headers "Range" ("bytes=" ^ range)
      | None, Some l ->
         Cohttp.Header.add headers "Range" ("bytes=-" ^ string_of_int l)
      | None, None -> headers in
    let u =
      Uri.of_string("https://api-content.dropbox.com/1/files/auto/" ^ fn) in
    let u = match rev with None -> u
                         | Some r -> Uri.with_query u ["rev", [r]] in
    Client.get ~headers u
    >>= check_errors_404 stream_of_file
end
