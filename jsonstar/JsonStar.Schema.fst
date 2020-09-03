module JsonStar.Schema

open JsonStar.Utils
open JsonStar.Json

let enrichWithCommon (s : schema) (j : json{JObject? j}) : Tot (z:json{JObject? z}) = 
    j
    ||> addPropOpt "$id" (Option.mapTot JString s._id)
    ||> addPropOpt "$schema" (Option.mapTot JString s._schema)
    ||> addPropOpt "description" (Option.mapTot JString s.description)
    ||> addPropOpt "title" (Option.mapTot JString s.title)
    ||> addPropOpt "default" (Option.mapTot JString s._default)
    // TODO: adding definitions and dependencies is more complicated

/// Converts json-schema representation into printable json
let toJson (s : schema) : Tot (z:json{JObject? z}) = 
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
            let maximum = Option.mapTot (fun (v:string) -> JString v) options.maximum in
            let minimum = Option.mapTot (fun (v:string) -> JString v) options.minimum in
            JObject [ "type", JString "number" ] // integer?
            ||> addPropOpt "maximum" maximum
            ||> addPropOpt "minimum" minimum
        | Boolean -> JObject [ "type", JString "boolean" ]
        // TODO: Replace with actual implementation
        | Object props deps options -> JObject [ "type", JString "object" ]
        | Array items options -> JObject [ "type", JString "array" ]
        | Reference ref -> JObject [ "$ref", JString ref ]
        | OneOf items -> JObject [ "oneOf", JArray []]
    in
    enrichWithCommon s j
