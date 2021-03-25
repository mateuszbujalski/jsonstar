module JsonStar.Schema

open JsonStar.Utils
open JsonStar.Json

let rec max_list (xs : list int{FStar.List.Tot.length xs >= 1}) : Tot int =
    match xs with
    | [ x ] -> x
    | x :: tl -> 
        let max_tail = max_list tl in
        if x > max_tail then x else max_tail

let empty_list_to_option (xs : list 'a) : Tot (option (y:list 'a{FStar.List.Tot.length y >= 1})) =
    match xs with
    | [] -> None
    | _ -> Some xs

let option_default (def : 'a) (opt : option 'a) : Tot 'a = 
    match opt with
    | None -> def
    | Some x -> x

let rec height (s : schema) : Tot int (decreases (Mkschema?._type s))=
    match s._type with
    | String _ -> 1
    | Enum _ -> 1
    | Integer -> 1
    | Number _ -> 1
    | Boolean -> 1
    | Reference _ -> 1
    | Array items _ -> 1 + (height items)
    | Object props deps _ -> 
        let props_heights = list_map props (fun (_, ss) -> height ss) in
        let deps_heights = list_map deps (fun (_, d) -> max_list (option_default [ 0 ] (empty_list_to_option (list_map d (fun (_, ss) -> height ss))))) in
        let props_max = max_list (option_default [ 0 ] (empty_list_to_option props_heights)) in
        let deps_max = max_list (option_default [ 0 ] (empty_list_to_option deps_heights)) in
        1 + (if props_max >= deps_max then props_max else deps_max)
    | OneOf items -> 
        let items_heights = list_map items (fun ss -> height ss) in
        1 + (max_list (option_default [ 0 ] (empty_list_to_option items_heights)))

let string_of_number (x : number) = 
    match x with
    | Int i -> string_of_int i

let mkSchemaEmpty (typ : schema_type) : Tot schema = 
    Mkschema None None typ None None None None None []

let defaultWith (def : 'a) (v : option 'a) = 
    match v with
    | Some x -> x
    | None   -> def

let enrichWithCommon (s : schema) (j : json{JObject? j}) : Tot (z:json{JObject? z}) = 
    j
    ||> addPropOpt "$id" (Option.mapTot JString s._id)
    ||> addPropOpt "$schema" (Option.mapTot JString s._schema)
    ||> addPropOpt "description" (Option.mapTot JString s.description)
    ||> addPropOpt "title" (Option.mapTot JString s.title)
    ||> addPropOpt "default" (Option.mapTot JString s._default)

val list_unref_pair : #a:Type -> #p:(a -> Type0) -> list (string * x:a{p x}) -> Tot (list (string * a))
let rec list_unref_pair #a #p l =
    match l with
    | [] -> []
    | (s, x)::xs -> (s, x) :: list_unref_pair xs

// let extendProp (key : string) (propKey :string) (propValue : json) (j:json{JObject? j) (JObject?.props j)}) : Tot (r:json{JObject? r}) =
//     let rec aux (ps : list (string * json) : Tot (list (string * json)) =
//         match ps with
//         | [] -> (propKey, propValue)
//         | p :: pss -> if (fst p) = key then (propKey, propValue) :: 
//     let props = Object?.props j in

// FIXME: A dirty workaround for now
let addToProperties (propKey : string) (propValue : json) (j : json{JObject? j}) : Tot (r:json{JObject? r}) = 
    let rec aux (ps : list (string *json)) : Tot (list (string * json)) =
        match ps with
        | [] -> [ "properties", JObject [ propKey, propValue ] ]
        | p :: pss -> 
            if (fst p) = "properties" && JObject? (snd p)
                then (fst p, addProp propKey propValue (snd p)) :: pss
                else p :: aux pss // BUG: What if JObject? (snd p) isn't true
    in
    JObject (aux (JObject?.props j))
                

// TODO: Prove the termination and split the "toJson" into smaller pieces
// let rec objectToJson (props:list (string * schema)) (deps: list (string * list (string * schema))) (options:object_options) : Tot (z:json{JObject? z}) = 
//     let required = Option.mapTot (fun (v:list string) -> JArray (List.Tot.map JString v)) options.required in
//     let additionalProp = JBoolean (defaultWith false options.additionalProperties) in 
//     let properties = list_map props (fun (name, prop_schema) -> name, (toJson prop_schema)) in
//     let deps_j : list (string * list (string * x:json{JObject? x})) = list_map deps (fun (name, dep_schema) -> name, list_map dep_schema (fun (value, ss) -> value, toJson ss)) in
//     let dependencies = 
//         list_map
//             deps
//             (fun (name, dep_schema) -> 
//                 let subs = 
//                     list_map 
//                         dep_schema 
//                         (fun (value, ss) ->
//                             let ss_j : x:json{JObject? x} = toJson ss in
//                             match ss_j with
//                             | JObject props -> JObject ((name, JObject [("enum", JArray [JString value])]) :: props)
//                         )
//                 in
//                 name, JObject ["oneOf", JArray subs] 
//             ) 
//     in
//     let dependencies_opt =
//         if FStar.List.Tot.length dependencies = 0 then 
//             None 
//         else Some (JObject dependencies)
//     in
//     JObject [ "type", JString "object" ]
//     ||> addPropOpt "dependencies" (dependencies_opt)
//     ||> addPropOpt "required" required
//     ||> addProp "additionalProperties" additionalProp
//     ||> addProp "properties" (JObject (list_unref_pair properties))


/// Converts json-schema representation into printable json
let rec toJson (s : schema) : Tot (z:json{JObject? z}) (decreases (height s)) = 
    let j =
        match s._type with
        | String options -> begin 
            let minLength = Option.mapTot (fun (v:nat) -> JNumber (string_of_int v) JInt) options.minLength in
            let maxLength = Option.mapTot (fun (v:nat) -> JNumber (string_of_int v) JInt) options.maxLength in
            let pattern = Option.mapTot JString options.pattern in
            let datao = Option.mapTot JString options.reference in

            let r = 
                (JObject [ "type", JString "string" ])
                ||> addPropOpt "minLength" minLength
                ||> addPropOpt "maxLength" maxLength
                ||> addPropOpt "pattern" pattern
            in
            match datao with
            | Some data ->
                r 
                ||> addProp "$type" (JString "reference")
                ||> addProp "$data" data
            | None -> r
            end
        | Enum items -> begin
            let enum_items = JArray (List.Tot.map (fun it -> JString it) items) in
            (JObject [ "type", JString "string"])
            ||> addProp "enum" enum_items
            end
        | Integer -> JObject [ "type", JString "integer" ]
        | Number options ->
            // TODO: Does it matter if it's represented as a number instead?
            let maximum = Option.mapTot (fun (v:number) -> JString (string_of_number v)) options.maximum in
            let minimum = Option.mapTot (fun (v:number) -> JString (string_of_number v)) options.minimum in
            JObject [ "type", JString "integer" ] // TODO: number? F* doesn't support anything except integers currently
            ||> addPropOpt "maximum" maximum
            ||> addPropOpt "minimum" minimum
        | Boolean -> JObject [ "type", JString "boolean" ]
        | Object props deps options -> begin
            let required = Option.mapTot (fun (v:list string) -> JArray (List.Tot.map JString v)) options.required in
            let additionalProp = JBoolean (defaultWith false options.additionalProperties) in 
            let properties = list_map props (fun (name, prop_schema) -> name, (toJson prop_schema)) in
            let deps_j : list (string * list (string * x:json{JObject? x})) = list_map deps (fun (name, dep_schema) -> name, list_map dep_schema (fun (value, ss) -> value, toJson ss)) in
            let dependencies = 
                list_map
                    deps
                    (fun (name, dep_schema) -> 
                        let subs = 
                            list_map 
                                dep_schema 
                                (fun (value, ss) ->
                                    let ss_j : x:json{JObject? x} = toJson ss in
                                    let ss_j_ex : x:json = addToProperties name (JObject [("enum", JArray [JString value])]) ss_j in
                                    ss_j_ex
                                    //match ss_j with
                                    //// BUG: the "enum" field should go into "properties" property inside "props"
                                    //// TODO: Rewrite the code to get a guarantee that "props" contains "properties" property
                                    ////       - extract the code that handles Object schema type and make it return json with 
                                    ////         and extra refinement about "properties" 
                                    ////       - Add some extra refinements to "deps" in schema definition (as it should contain a schema of a record)
                                    ////         and make it work with tactics (might be hard as it's defined with mutually recursive types and using the other type in a refinement of the first one doesn't seem supported)
                                    //| JObject props -> JObject ((name, JObject [("enum", JArray [JString value])]) :: props)
                                )
                        in
                        name, JObject ["oneOf", JArray subs] 
                    ) 
            in
            let dependencies_opt =
                if FStar.List.Tot.length dependencies = 0 then 
                    None 
                else Some (JObject dependencies)
            in
            JObject [ "type", JString "object" ]
            ||> addPropOpt "dependencies" (dependencies_opt)
            ||> addPropOpt "required" required
            ||> addProp "additionalProperties" additionalProp
            ||> addProp "properties" (JObject (list_unref_pair properties))
            end
        | Array items options -> begin
            let minItems = Option.mapTot (fun (v:nat) -> JNumber (string_of_int v) JInt) options.minItems in
            let maxItems = Option.mapTot (fun (v:nat) -> JNumber (string_of_int v) JInt) options.maxItems in
            let uniqueItems = Option.mapTot (fun (v:bool) -> JBoolean v) options.uniqueItems in
            JObject [ "type", JString "array" ]
            ||> addPropOpt "minItems" minItems
            ||> addPropOpt "maxItems" maxItems
            ||> addPropOpt "uniqueItems" uniqueItems
            ||> addProp "items" (toJson items)
            end
        | Reference ref -> JObject [ "$ref", JString ref ]
        | OneOf items -> JObject [ "oneOf", JArray (list_map items toJson)]
    in
    let definitions : option (list (string * x:json{JObject? x})) = 
        if FStar.List.Tot.length s.definitions = 0 then 
            None 
        else Some (list_map s.definitions (fun (name, def_schema) -> name, (toJson def_schema))) 
    in
    j
    ||> addPropOpt "definitions" (Option.mapTot (fun d -> JObject (list_unref_pair d)) definitions)
    ||> enrichWithCommon s
