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
