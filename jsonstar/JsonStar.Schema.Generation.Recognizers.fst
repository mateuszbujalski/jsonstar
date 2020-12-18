module JsonStar.Schema.Generation.Recognizers

module T = FStar.Tactics
module L = FStar.List.Tot

let primitives = [ "Prims.string"; "Prims.int"; "Prims.bool" ]

type term_type = 
    | Primitive : t:T.term -> term_type
    | Refinement : t:T.term -> term_type
    | Enum : t:T.fv -> term_type
    | Record : JsonStar.Schema.Generation.Record.record -> term_type
    | Unrecognized : t:T.term -> term_type

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
            Helpers.satisfy_all (fun (_, ct) -> Helpers.termeq (T.pack (T.Tv_FVar fv)) ct) cts
            end
        | T.Unk -> false // TODO: What's Unk? Should we fail here?
        end
    | None -> false

// Adds "Mk" to last segment of the qualified name
let rec record_constructor_name (n : list string{Cons? n}) : Tot (list string) = 
    match n with
    | [ x ] -> [ "Mk" ^ x ]
    | x :: xs -> x :: record_constructor_name xs

// Records are syntactic sugar over inductive types
let isRecord (fv : T.fv) : T.Tac bool = 
    let env = T.cur_env () in
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Let _ _ _ _ _ -> false
        | T.Sg_Inductive _ _ _ _ cts -> begin
            // A record should have exactly one constructor with at least one field
            // and the name of that constructor should be "Mk{record_type}"
            match cts with
            | [ (ctname, ct) ] -> 
                begin match T.inspect ct with 
                | T.Tv_Arrow _ _ -> (T.implode_qn ctname) = (T.implode_qn (record_constructor_name qname))
                | _ -> false
                end
            | _ -> false
            end
        | T.Unk -> false
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
        else if isRecord fv then
            Record (JsonStar.Schema.Generation.Record.get_record (T.cur_env ()) (T.inspect_fv fv))
        else Unrecognized t
        end
    | T.Tv_Refine _ _ -> Refinement t
    | _               -> Unrecognized t
