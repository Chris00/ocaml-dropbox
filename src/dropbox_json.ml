(* This module gathers parsing functions to supplement atdgen. *)

module Date = Dropbox_date

let float_of_json = function
  | `Float f -> f
  | `Int i -> float i

module Photo = struct
    type info = { time_taken: Date.t option;
                  lat_long: (float * float) option }

    type t = [ `None | `Pending | `Some of info ]

    let set_info time_taken lat_long = function
      | ("time_taken", `String d) ->
         time_taken := Some(Date.of_string d)
      | ("lat_long", (`List [(`Float _ | `Int _) as la;
                             (`Float _ | `Int _) as lo]
                     | `Tuple [(`Float _ | `Int _) as la;
                               (`Float _ | `Int _) as lo])) ->
         lat_long := Some (float_of_json la, float_of_json lo)
      | _ -> ()

    let wrap : Yojson.Safe.json -> t = function
      | `String _ -> `Pending
      | `Assoc l ->
         let time_taken = ref None in
         let lat_long = ref None in
         List.iter (set_info time_taken lat_long) l;
         `Some { time_taken = !time_taken;  lat_long = !lat_long }
      | _ -> `None

    let unwrap : t -> Yojson.Safe.json = function
      | `None -> `Null
      | `Pending -> `String "pending"
      | `Some info ->
         let l = match info.time_taken with
           | Some d -> ["time_taken", `String(Date.to_string d)]
           | None -> [] in
         let l = match info.lat_long with
           | Some(la, lo) -> ("lat_long", `List [`Float la; `Float lo]) :: l
           | None -> l in
         `Assoc l
  end

module Video = struct
    type info = { time_taken: Date.t option;
                  duration: float option;
                  lat_long: (float * float) option }

    type t = [ `None | `Pending | `Some of info ]

    let set_info time_taken duration lat_long = function
      | ("time_taken", `String d) ->
         time_taken := Some(Date.of_string d)
      | ("duration", (`Float _ | `Int _ as d)) ->
         duration := Some(float_of_json d)
      | ("lat_long", (`List [(`Float _ | `Int _) as la;
                             (`Float _ | `Int _) as lo]
                     | `Tuple [(`Float _ | `Int _) as la;
                               (`Float _ | `Int _) as lo])) ->
         lat_long := Some (float_of_json la, float_of_json lo)
      | _ -> ()

    let wrap : Yojson.Safe.json -> t = function
      | `String _ -> `Pending
      | `Assoc l ->
         let time_taken = ref None in
         let duration = ref None in
         let lat_long = ref None in
         List.iter (set_info time_taken duration lat_long) l;
         `Some { time_taken = !time_taken;
                 duration = !duration;
                 lat_long = !lat_long }
      | _ -> `None

    let unwrap : t -> Yojson.Safe.json = function
      | `None -> `Null
      | `Pending -> `String "pending"
      | `Some info ->
         let l = match info.time_taken with
           | Some d -> ["time_taken", `String(Date.to_string d)]
           | None -> [] in
         let l = match info.duration with
           | Some d -> ("duration", `Float d) :: l
           | None -> l in
         let l = match info.lat_long with
           | Some(la, lo) -> ("lat_long", `List [`Float la; `Float lo]) :: l
           | None -> l in
         `Assoc l
  end

module Link = struct
  type info = {url : string;
               expires: Date.t;
               visibility: string}

  type t = [
    | `None
    | `Public of info
    | `Team_only of info
    | `Team_and_password of info
    | `Shared_folder_only of info
    | `Unknown of info
    ]

  let set_info url expires visibility = function
    | ("url", `String d) -> url := d
    | ("expires", `String d) -> expires := (Date.of_string d)
    | ("visibility", `String d) -> visibility := d
    | _ -> ()

  let wrap : Yojson.Safe.json -> t = function
    | `Assoc l ->
       let url = ref "" in
       let expires = ref (Date.of_string "") in
       let visibility = ref "" in
       List.iter (set_info url expires visibility) l;
       let dict = { url = !url;
                    expires = !expires;
                    visibility = !visibility } in
       (match !visibility with 
       | "PUBLIC" -> `Public dict
       | "TEAM_ONLY" -> `Team_only dict
       | "TEAM_AND_PASSWORD" -> `Team_and_password dict
       | "SHARED_FOLDER_ONLY" -> `Shared_folder_only dict
       | _ -> `Unknown dict )
    | _ -> `None

  let unwrap : t -> Yojson.Safe.json = function
    | `None -> `Null
    | `Public info | `Team_only info | `Team_and_password info
    | `Shared_folder_only info ->
       let l = [("visibility", `String info.visibility)] in
       let l = ("expires", `String(Date.to_string info.expires)) :: l in
       let l = ("url", `String (info.url)) :: l in
       `Assoc l
    | `Unknown info -> `Null
end
