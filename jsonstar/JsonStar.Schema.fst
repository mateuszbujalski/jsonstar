module JsonStar.Schema

open JsonStar.Utils
open JsonStar.Json

let string_of_number (x : number) = 
    match x with
    | Int i -> string_of_int i

let mkSchemaEmpty (typ : schema_type) : Tot schema = 
    Mkschema None None typ None None None []

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

/// Converts json-schema representation into printable json
let rec toJson (s : schema) : Tot (z:json{JObject? z}) = 
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
            let dependencies = list_map deps (fun (name, dep_schema) -> name, (toJson dep_schema)) in
            JObject [ "type", JString "object" ]
            ||> addPropOpt "required" required
            ||> addProp "additionalProperties" additionalProp
            ||> addProp "properties" (JObject properties)
            ||> addProp "dependencies" (JObject dependencies)
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
    let definitions = 
        if FStar.List.Tot.length s.definitions = 0 then 
            None 
        else Some (list_map s.definitions (fun (name, def_schema) -> name, (toJson def_schema))) 
    in
    j
    ||> addPropOpt "definitions" (Option.mapTot JObject definitions)
    ||> enrichWithCommon s
