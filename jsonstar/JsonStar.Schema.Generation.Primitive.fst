module JsonStar.Schema.Generation.Primitive

module T = FStar.Tactics
module L = FStar.List.Tot

//module Helpers = JsonStar.Schema.Generation.Helpers

open JsonStar.Schema

let mkPrimitiveSchema (s : schema_type) : Tot schema = 
    {
        _id = None;
        _schema = None;
        _type = s;
        description = None;
        title = None;
        _default = None;
        definitions = [];
    }

// 
let toSchema (t : T.term) : T.Tac schema =
    if T.term_eq t (`string) then mkPrimitiveSchema (String (mkempty_string_options ()))
    else if T.term_eq t (`int) then mkPrimitiveSchema (Number (Mknumber_options None None))
    else if T.term_eq t (`bool) then mkPrimitiveSchema Boolean
    else Helpers.tfail ((T.term_to_string t) ^ " is not a primitive type.\n")