module JsonStar.Json

open FStar.Pervasives.Native

let addProp (key:string) (v:json) (j:json{JObject? j}) : Tot (r:json{JObject? r}) =
    JObject ((key, v) :: (JObject?.props j))

let addPropOpt (key:string) (vo:option json) (j:json{JObject? j}) : Tot (r:json{JObject? r}) =
    match vo with
    | Some v -> addProp key v j
    | None -> j

// NOTE: There's some mismatch between the option type produced with extracted find from 
// FStar.List.Tot and option used in this file to pattern match. By copying the definition of find
// I can work around this for now. 
val find: #a:Type
        -> f:(a -> Tot bool)
        -> list a
        -> Tot (option (x:a{f x}))
let rec find #a f l = match l with
  | [] -> None #(x:a{f x}) //These type annotations are only present because it makes bootstrapping go much faster
  | hd::tl -> if f hd then Some #(x:a{f x}) hd else find f tl

let getProp (key : string) (j:json{JObject? j}) : Tot (option json) =
    let props = JObject?.props j in
    let prop_opt = find (fun (k,_) -> k = key) props in
    match prop_opt with
    | Some prop -> begin
        let (_, v) = prop in
        Some v
        end
    | None -> None
