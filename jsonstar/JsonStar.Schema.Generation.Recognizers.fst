module JsonStar.Schema.Generation.Recognizers

module T = FStar.Tactics
module L = FStar.List.Tot

let primitives = [ "Prims.string"; "Prims.int"; "Prims.bool" ]

type term_type = 
    | Primitive : t:T.term -> term_type
    | Refinement : t:T.term -> term_type
    | Enum : t:T.fv -> term_type
    | Unrecognized : t:T.term -> term_type

val satisfy_all: ('a -> T.Tac bool) -> list 'a -> T.Tac bool
let rec satisfy_all f x = match x with
  | [] -> true
  | a::tl -> if f a then satisfy_all f tl else false

let isEnum (fv : T.fv) : T.Tac bool = 
    let env = T.cur_env () in
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Let _ _ _ _ _ -> false
        | T.Sg_Inductive _ _ _ _ cts -> begin
            // check if all constructor types are simply the type of the enum
            satisfy_all (fun (_, ct) -> Helpers.termeq (T.pack (T.Tv_FVar fv)) ct) cts
            end
        | T.Unk -> false // TODO: What's Unk? Should we fail here?
        end
    | None -> false

let term_to_type (t : T.term) : T.Tac term_type =
    let tv : T.term_view = T.inspect t in
    match tv with 
    | T.Tv_FVar fv -> begin
        if L.mem (T.fv_to_string fv) primitives then 
            Primitive t
        else if isEnum fv then
            Enum fv
        else Unrecognized t
        end
    | T.Tv_Refine _ _ -> Refinement t
    | _               -> Unrecognized t
