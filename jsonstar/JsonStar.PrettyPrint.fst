module JsonStar.PrettyPrint

open FStar.String
open JsonStar.Json

// F* has troubles figuring out "decreases" for inductive types that use functions like map or fold:
// https://github.com/FStarLang/FStar/issues/463 
// Also see ('treeMap' definition)
// https://github.com/FStarLang/FStar/blob/master/examples/termination/termination.fst#L110
// Here with ({x<<xs}) we explicitly say that f accepts only arguments smaller than 'xs'
val list_map : (xs:list 'a) -> (f : (x : 'a{x<<xs} -> 'b)) -> Tot (list 'b)
let rec list_map xs f =
	match xs with 
	| [] -> []
	| x :: xs' -> f x :: list_map xs' f

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


