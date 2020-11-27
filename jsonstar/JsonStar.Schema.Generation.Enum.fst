module JsonStar.Schema.Generation.Enum

open JsonStar.Schema
module T = FStar.Tactics
module L = FStar.List.Tot

let get_enum_constructors (fv : T.fv) : T.Tac (list (list string)) = 
    let env = T.cur_env () in
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Inductive _ _ _ _ cts -> T.map (fun (name, _) -> name) cts
        | _ -> Helpers.tfail ("Expected enum, got " ^ (T.term_to_string (T.pack (T.Tv_FVar fv))))
        end
    | None -> Helpers.tfail ("Could not find type " ^ (T.implode_qn qname))

let pack_enum_constructor (name : list string) : T.Tac T.term =
    T.pack (T.Tv_FVar (T.pack_fv name))

let unpack_enum_constructor (t : T.term) : T.Tac (list string) =
    match T.inspect t with
    | T.Tv_FVar fv -> T.inspect_fv fv
    | _ -> T.fail ("Expected enum constructor name, got " ^ (T.term_to_string t))

let toSchema (fv : T.fv) : T.Tac schema =
    let values = T.map (fun n -> L.last n) (get_enum_constructors fv) in
    {
        _id = None;
        _schema = None;
        _type = Enum values;
        description = None;
        title = None;
        _default = None;
        definitions = [];
    }
