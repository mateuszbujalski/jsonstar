module JsonStar.Json

type number_type = 
    | JInt
    | JFloat

type json = 
    | JNull : json
    | JBoolean : v:bool -> json
    | JNumber : v:string -> number_type -> json
    | JString : v:string -> json
    | JArray : items:list json -> json
    | JObject : props:list (string * json) -> json

val addProp : (key:string) -> (v:json) -> (j:json{JObject? j}) -> Tot (r:json{JObject? r})
val addPropOpt : (key:string) -> (v:option json) -> (j:json{JObject? j}) -> Tot (r:json{JObject? r})
