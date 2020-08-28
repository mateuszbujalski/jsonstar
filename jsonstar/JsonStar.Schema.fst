module JsonStar.Schema

open JsonStar.Utils
open JsonStar.Json

let toJson (s : schema) : Tot json = 
    match s._type with
    | String options -> begin 
        let minLength = Option.mapTot (fun (v:nat) -> JNumber (string_of_int v) JInt) options.minLength in
        let maxLength = Option.mapTot (fun (v:nat) -> JNumber (string_of_int v) JInt) options.maxLength in
        let pattern = Option.mapTot JString options.pattern in
        let datao = Option.mapTot JString options.reference in
        // TODO: Rewrite with pipe? Do notation for building JObject?
        let r1 = addPropOpt "minLength" minLength (JObject [ "type", JString "string" ]) in
        let r2 = addPropOpt "maxLength" maxLength r1 in 
        let r3 = addPropOpt "pattern" pattern r2 in
        match datao with
        | Some data -> 
            let r4 = addProp "$type" (JString "reference") r3 in
            addProp "$data" data r4
        | None -> r3
        end
    | _ -> JNull
