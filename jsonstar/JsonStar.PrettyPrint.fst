module JsonStar.PrettyPrint

open FStar.String
open JsonStar.Json
open JsonStar.Utils

// TODO: Pretty print
let rec stringify (j : json) =
	match j with
	| JNull -> "null"
	| JBoolean b -> "\"" ^ string_of_bool b ^ "\""
	| JNumber n _ -> "\"" ^ n ^ "\""
	| JString s -> "\"" ^ s ^ "\""
	| JArray xs ->
		let vs = list_map xs (fun x -> stringify x) in
		concat " " [ "["; concat ", " vs; "]" ]
	| JObject xs ->
		let props = list_map xs (fun (key, value) -> concat " " [ "\"" ^ key ^ "\""; ":"; stringify value ]) in
		concat " " [ "{"; concat ", " props; "}" ]


