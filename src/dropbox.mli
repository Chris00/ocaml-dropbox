(** Binding to the Dropbox
    {{:https://www.dropbox.com/developers/core/docs}Remote API}. *)

type error_description = { error: string;
                           error_description: string }

type error =
  | Invalid_arg of error_description
  (** Bad input parameter.  The string should indicate why. *)
  | Invalid_token of error_description
  (** Bad or expired token. This can happen if the user or Dropbox
      revoked or expired an access token. To fix, you should
      re-authenticate the user. *)
  | Invalid_oauth of error_description
  (** Bad OAuth request (wrong consumer key, bad nonce, expired
      timestamp...). Unfortunately, re-authenticating the user won't
      help here. *)
  | Too_many_requests of error_description
  (** Your app is making too many requests and is being rate limited.
      [Too_many_requests] can trigger on a per-app or per-user
      basis. *)
  | Try_later of int option * error_description
  (** [Try_later(sec, e)] If [sec = Some s], this means your app is
      being rate limited and you must retry after [s] seconds.
      Otherwise, this indicates a transient server error, and your app
      should retry its request. *)
  | Quota_exceeded of error_description
  (** User is over Dropbox storage quota. *)
  | Server_error of int * error_description

val string_of_error : error -> string

exception Error of error

(** Date representation. *)
module Date : sig
  type t = Dropbox_date.t

  (** Day of week. *)
  type wday = Dropbox_date.wday
            = Sun | Mon | Tue | Wed | Thu | Fri | Sat

  (** Month. *)
  type month
    = Dropbox_date.month
    = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

  val day : t -> int      (** Day of the month (1-31). *)
  val month : t -> month  (** Month *)
  val year : t -> int     (** 4 digits year *)
  val hour : t -> int     (** Hour *)
  val min : t -> int      (** Minutes *)
  val sec : t -> int      (** Seconds *)
  val wday : t -> wday    (** Day of week *)

  val to_string : t -> string
end

(** Dropbox API. *)
module type S = sig

  (** {{:http://oauth.net/}OAuth 2.0} authentication. *)
  module OAuth2 : sig

    val authorize : ?state: string ->
                    ?force_reapprove: bool ->
                    ?disable_signup: bool ->
                    id: string ->
                    [`Token of Uri.t | `Code of Uri.t option] -> Uri.t
    (** [authorize client_id response] starts the OAuth 2.0 authorization flow.
        This isn't an API call—it's the web page that lets the user sign
        in to Dropbox and authorize your app.  The [client_id] is the
        app's key, found in the
        {{:https://www.dropbox.com/developers/apps}App Console}.  After
        the user authorizes your app, they will be sent to your redirect
        URI.  The type of response varies based on the [response]:

        - [`Token redirect_uri] (also called "implicit grant") returns
          the bearer token by redirecting the user to [redirect_uri]
          after the authorization has completed.  Extract the token
          using {!token_of_uri}.  This is useful for pure client-side
          apps, such as mobile apps or JavaScript-based apps.

        - [`Code u] if [u = Some redirect_uri], returns a code via by
          redirecting the user to [redirect_uri] (extract the code
          using {!code_of_uri}) or, if [u = None], presents the code
          to use user (on screen) who will be invited to copy it in
          your app.  The code should then be converted into a bearer
          token using {!OAuth2.token}.  This is recommended for apps
          that are running on a server.

        Note that the URI for [`Token] and [`Code] must be registered
        in the {{:https://www.dropbox.com/developers/apps}App
        Console}; even 'localhost' must be listed if it is used for
        testing.

        @param state Up to 200 bytes of arbitrary data that will be
        passed back to your redirect URI. This parameter should be used
        to protect against cross-site request forgery (CSRF).  See
        Sections
        {{:http://tools.ietf.org/html/rfc6819#section-4.4.1.8}4.4.1.8}
        and
        {{:http://tools.ietf.org/html/rfc6819#section-4.4.2.5}4.4.2.5}
        of the OAuth 2.0 threat model spec.

        @param force_reapprove Whether or not to force the user to
        approve the app again if they've already done so. If [false]
        (default), a user who has already approved the application may
        be automatically redirected to the URI specified by
        [redirect_uri].  If [true], the user will not be automatically
        redirected and will have to approve the app again.

        @param disable_signup When [true] (default is [false]) users
        will not be able to sign up for a Dropbox account via the
        authorization page.  Instead, the authorization page will show a
        link to the Dropbox iOS app in the App Store.  This is only
        intended for use when necessary for compliance with App Store
        policies.  *)

    type code = string
    (** The authorization code, which can be used to attain
        a bearer token by calling {!token}.  *)

    val code_of_uri : Uri.t -> (code * string) option
    (** [code_of_uri u] return the code and state from the redirect
        URI [u] after a [`Code] authorization. *)

    type token = string

    val token_of_uri : Uri.t -> (token * string) option
    (** [token_of_uri u] parse the URI coming from a [`Token] flow and
        extract the token and state. *)

    val token : ?redirect_uri: Uri.t ->
                code -> id: string -> secret: string -> token Lwt.t
    (** [token code id secret] acquire a token once the user has
        authorized the app.  Only applies to apps using the
        authorization [`Code] flow.

        [code] is the code acquired by directing users to
        [OAuth2.authorize ~response_type:`Code].

        [id] this should be the app's key (found in the
        {{:https://www.dropbox.com/developers/apps}App Console}).

        [secret] this parameter should be present and should be the
        app's secret.

        @param redirect_uri Only used to validate that it matches the
        original {!authorize}, not used to redirect again.  *)
  end

  type t
  (** Represent a session communicating with Dropbox. *)

  val session : OAuth2.token -> t

  val token : t -> OAuth2.token
  (** The token of the current session. *)

  type name_details
    = Dropbox_t.name_details
    = { familiar_name: string; (** The locale-dependent familiar name
                                   for the user. *)
        given_name: string; (** The user's given name. *)
        surname: string;    (** The user's surname. *)
      }

  type team = Dropbox_t.team
            = { name: string; (** The name of the team the user belongs to. *)
                team_id: int; (** The ID of the team the user belongs to. *)
              }

  type quota_info
    = Dropbox_t.quota_info
    = { shared: int; (** The user's used quota outside of shared
                         folders (bytes). *)
        quota: int;  (** The user's used quota in shared folders (bytes). *)
        normal: int; (** The user's total quota allocation (bytes). *)
      }

  type info
    = Dropbox_t.info
    = { uid: int; (** The user's unique Dropbox ID. *)
        display_name: string; (** The user's display name. *)
        email_verified: bool;
        name_details: name_details;
        referral_link: Uri.t;
        (** The user's {{:https://www.dropbox.com/referrals}referral link}. *)
        country: string;
        (** The user's two-letter country code, if available. *)
        locale: string; (** Locale preference set by the user (e.g. en-us). *)
        is_paired: bool;
        (** If true, there is a paired account associated with this user. *)
        team: team option;
        (** If the user belongs to a team, contains team information. *)
        quota_info: quota_info;
      }

  val info : ?locale: string -> t -> info Lwt.t
  (** [info t] return the information about the user's account.

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.  *)

  type photo_info
    = Dropbox_t.photo_info
    = { time_taken: Date.t option;
        lat_long: float list }

  type video_info
    = Dropbox_t.video_info
    = { time_taken: Date.t option;
        duration: float;
        lat_long: float list }

  type metadata = Dropbox_t.metadata = {
      size: string;
      (** A human-readable description of the file size (translated by
          locale). *)
      bytes: int;      (** The file size in bytes. *)
      mime_type: string;
      path: string;    (** Returns the canonical path to the file or folder. *)
      is_dir: bool;    (** Whether the given entry is a folder or not. *)
      is_deleted: bool; (** Whether the given entry is deleted.  (Only
                            interesting if deleted files are returned.)  *)
      rev: string;
      (** A unique identifier for the current revision of a file.  This
          field is the same [rev] as elsewhere in the API and can be
          used to detect changes and avoid conflicts. *)
      hash: string;
      (** A folder's hash is useful for indicating changes to the
          folder's contents in later calls to {!metadata}.  This is
          roughly the folder equivalent to a file's [rev].  (Is [""]
          for a file.) *)
      thumb_exists: bool;
      (** True if the file is an image that can be converted to a
          thumbnail via the {!thumbnails} call. *)
      photo_info: photo_info option;
      (** Only returned when the include_media_info parameter is true and the
          file is an image. A dictionary that includes the creation time
          (time_taken) and the GPS coordinates (lat_long). *)
      video_info: video_info option;
      (** Only returned when the include_media_info parameter is true and the
          file is a video. A dictionary that includes the creation time
          (time_taken), the GPS coordinates (lat_long), and the length of the
          video in milliseconds (duration). *)
      icon: string;
      (** The name of the icon used to illustrate the file type in Dropbox's
          {{:https://www.dropbox.com/static/images/dropbox-api-icons.zip}icon
          library}. *)
      modified: Date.t;
      (** The last time the file was modified on Dropbox (not included
          for the root folder).  *)
      client_mtime: Date.t option;
      (** For files, this is the modification time set by the desktop
          client when the file was added to Dropbox.  Since this time
          is not verified (the Dropbox server stores whatever the
          desktop client sends up), this should only be used for
          display purposes (such as sorting) and not, for example, to
          determine if a file has changed or not. *)
      root: [ `Dropbox | `App_folder ];
      (** The root or top-level folder depending on your access
          level. All paths returned are relative to this root level. *)
      contents: metadata list
    }

  type delta = Dropbox_t.delta = {
      entries: (string * metadata) list;
      (** A list of "delta entries". Each delta entry is a 2-item list of one
          of the following forms:

          [<path>, <metadata>] - Indicates that there is a file/folder at the
          given path. You should add the entry to your local state. The
          metadata value is the same as what would be returned by the
          /metadata call, except folder metadata doesn't have hash or contents
          fields. To correctly process delta entries:

          - If the new entry includes parent folders that don't yet exist in
            your local state, create those parent folders in your local state.

          - If the new entry is a file, replace whatever your local state has
            at path with the new entry.

          - If the new entry is a folder, check what your local state has at
            <path>. If it's a file, replace it with the new entry. If it's a
            folder, apply the new <metadata> to the folder, but don't modify
            the folder's children. If your local state doesn't yet include
            this path, create it as a folder.

          - If the new entry is a folder with the read-only field set to true,
            apply the read-only permission recursively to all files within the
            shared folder.

          [<path>, null] - Indicates that there is no file/folder at the given
          path. To update your local state to match, anything at path and all
          its children should be deleted. Deleting a folder in your Dropbox
          will sometimes send down a single deleted entry for that folder, and
          sometimes separate entries for the folder and all child paths. If
          your local state doesn't have anything at path, ignore this entry. *)
      reset: bool;
      (** If true, clear your local state before processing the delta entries.
          reset is always true on the initial call to /delta  (i.e. when no
          cursor is passed in). Otherwise, it is true in rare situations,
          such as after server or account maintenance, or if a user deletes
          their app folder. *)
      cursor: string;
      (** A string that encodes the latest information that has been returned.
          On the next call to /delta, pass in this value. *)
      has_more: bool
      (** If true, then there are more entries available; you can call /delta
          again immediately to retrieve those entries. If 'false', then wait for
          at least five minutes (preferably longer) before checking again. *)
      }

  type longpoll_delta
    = Dropbox_t.longpoll_delta
    = { changes: bool; (** Incidate whether new changes are available *)
        backoff: int   (** If present, it indicates how many seconds you code
                           should wait before calling /longpoll_delta again *)
      }

  val get_file : t -> ?rev: string -> ?start: int -> ?len: int ->
                 string -> (metadata * string Lwt_stream.t) option Lwt.t
  (** [get_file t name] return the metadata for the file and a stream of
      its content.  [None] indicates that the file does not exists.

      @param start The first byte of the file to download.  A negative
      number is interpreted as [0].  Default: [0].

      @param len The number of bytes to download.  If [start] is not set,
      the last [len] bytes of the file are downloaded.  Default: download
      the entire file (or everything after the position [start],
      including [start]).  If [start <= 0], the metadata will be present
      but the stream will be empty. *)

  val delta : ?cursor: string -> ?locale: string -> ?path_prefix: string
              -> ?include_media_info: bool -> t -> delta Lwt.t
  (** [delta t] return the JSON object delta.

      @param cursor A string that is used to keep track of your current state.
      On the next call pass in this value to return delta entries that have
      been recorded since the cursor was returned.

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.

      @param path_prefix If present, this parameter filters the response
      to only include entries at or under the specified path. For example,
      a path_prefix of "/Photos/Vacation" will return entries for the path
      "/Photos/Vacation" and any files and folders under that path. If you
      use the path_prefix parameter, you must continue to pass the correct
      prefix on subsequent calls using the returned cursor. You can switch
      the path_prefix on any existing cursor to a descendant of the existing
      path_prefix on subsequent calls to /delta. For example if your cursor
      has no path_prefix, you can switch to any path_prefix. If your cursor
      has a path_prefix of "/Photos", you can switch it to "/Photos/Vacaction".

      @param include_media_info If true, each file will include a photo_info
      dictionary for photos and a video_info dictionary for videos with
      additional media info. When include_media_info is specified, files will
      only appear in delta responses when the media info is ready. If you use
      the include_media_info parameter, you must continue to pass the same
      value on subsequent calls using the returned cursor. *)

  val latest_cursor : ?path_prefix: string -> ?include_media_info: bool
                      -> t -> delta Lwt.t
  (** [latest_cursor t] return the JSON object delta with only the
      field cursor as would be returned by /delta when has_more is false.

      @param path_prefix If present, the returned cursor will be encoded with
      a path_prefix for the specified path for use with /delta.

      @param include_media_info If true, the returned cursor will be encoded
      with include_media_info set to true for use with /delta. *)

  val longpoll_delta : t -> ?timeout: int -> string -> longpoll_delta Lwt.t
  (** [longpoll_delta t] return the JSON object longpoll_delta. The connection
      will block until there are changes available or a timeout occurs.

      @param cursor A delta cursor as returned from a call to /delta. Note
      that a cursor returned from a call to /delta with include_media_info=true
      is incompatible with /longpoll_delta and an error will be returned.

      @param timeout An optional integer indicating a timeout, in seconds. The
      default value is 30 seconds, which is also the minimum allowed value.
      The maximum is 480 seconds. The request will block for at most this
      length of time, plus up to 90 seconds of random jitter added to avoid
      the thundering herd problem. Care should be taken when using this
      parameter, as some network infrastructure does not support long timeouts.

      Possible errors:
      400 One or more parameters were invalid. The response will be of the
      form {"error": "<reason>"}. *)
  ;;
end

module Make(Client: Cohttp_lwt.Client) : S
(** Create a concrete Dropbox API implementation given a client one.
    Note that several instances have been instantiated for you in the
    Dropbox_* modules so you generally do not have to call this
    yourself. *)
;;
