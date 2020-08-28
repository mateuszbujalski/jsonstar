module JsonStar.Json

let addProp (key:string) (v:json) (j:json{JObject? j}) : Tot (r:json{JObject? r}) =
    JObject ((key, v) :: (JObject?.props j))

let addPropOpt (key:string) (vo:option json) (j:json{JObject? j}) : Tot (r:json{JObject? r}) =
    match vo with
    | Some v -> addProp key v j
    | None -> j
