module JsonStar.Json

type number_type = 
    | JInt
    | JFloat

type json = 
    | JNull : json
    | JBoolean : bool -> json
    | JNumber : string -> number_type -> json
    | JString : string -> json
    | JArray : list json -> json
    | JObject : list (string * json) -> json