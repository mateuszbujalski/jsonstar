module JsonStar_Parser

open Newtonsoft.Json.Linq
open JsonStar_Json

let private parseJson(content : string) : JObject =
    JObject.Parse content

let rec private convert (j : JToken) : json =
    match j.Type with
    | JTokenType.Null -> JNull
    | JTokenType.Boolean -> JBoolean <| j.Value<bool>()
    | JTokenType.Integer -> JNumber (string (j.Value<int>()), JInt)
    | JTokenType.Float -> JNumber (string (j.Value<float>()), JFloat)
    | JTokenType.String -> JString <| j.Value<string>()
    | JTokenType.Array -> j.Children() |> Seq.map convert |> List.ofSeq |> JArray
    | JTokenType.Object -> j.Children() |> Seq.map (fun p -> let p = p :?> JProperty in p.Name, convert p.Value) |> List.ofSeq |> JObject
    | _ -> failwithf "Unexpected token %s" (string j.Type)

let parse (content : string) : json =
    content
    |> parseJson
    |> convert

let int_of_string (s : string) = System.Numerics.BigInteger.Parse s