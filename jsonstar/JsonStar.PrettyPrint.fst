module JsonStar.PrettyPrint

open FStar.String
open JsonStar.Json
open JsonStar.Utils

let bool_to_string (b : bool) : Tot string =
	if b then "true" else "false"

// TODO: Pretty print
let rec stringify (j : json) =
	match j with
	| JNull -> "null"
	| JBoolean b -> "\"" ^ bool_to_string b ^ "\""
	| JNumber n _ -> "\"" ^ n ^ "\""
	| JString s -> "\"" ^ s ^ "\""
	| JArray xs ->
		let vs = list_map xs (fun x -> stringify x) in
		concat " " [ "["; concat ", " vs; "]" ]
	| JObject xs ->
		let props = list_map xs (fun (key, value) -> concat " " [ "\"" ^ key ^ "\""; ":"; stringify value ]) in
		concat " " [ "{"; concat ", " props; "}" ]


