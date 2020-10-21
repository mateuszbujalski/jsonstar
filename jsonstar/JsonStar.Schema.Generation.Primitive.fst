module JsonStar.Schema.Generation.Primitive

module T = FStar.Tactics
module L = FStar.List.Tot

//module Helpers = JsonStar.Schema.Generation.Helpers

open JsonStar.Schema

let primitives = [ "Prims.string"; "Prims.int"; "Prims.bool" ]
let isPrimitive (tv : T.term_view) : Tot bool =
    match tv with
    | T.Tv_FVar fv -> L.mem (T.fv_to_string fv) primitives
    | _          -> false

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

let toSchema (t : T.term) : T.Tac (option schema) =
    if T.term_eq t (`string) then Some (mkPrimitiveSchema (String (mkempty_string_options ())))
    else if T.term_eq t (`int) then Some (mkPrimitiveSchema Integer)
    else if T.term_eq t (`bool) then Some (mkPrimitiveSchema Boolean)
    else None