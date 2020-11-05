module JsonStar.Schema.Generation.Recognizers

module T = FStar.Tactics
module L = FStar.List.Tot

let primitives = [ "Prims.string"; "Prims.int"; "Prims.bool" ]

type term_type = 
    | Primitive : t:T.term -> term_type
    | Refinement : t:T.term -> term_type
    | Unrecognized : t:T.term -> term_type

let term_to_type (t : T.term) : T.Tac term_type =
    let tv : T.term_view = T.inspect t in
    match tv with 
    | T.Tv_FVar fv when L.mem (T.fv_to_string fv) primitives -> Primitive t
    | T.Tv_Refine _ _ -> Refinement t
    | _               -> Unrecognized t
