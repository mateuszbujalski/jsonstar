module JsonStar.Schema.Generation.Record

module T = FStar.Tactics
module Helpers = JsonStar.Schema.Generation.Helpers

type record_field = 
    {
        name : string;
        attrs : option T.term;
        typ : T.term
    }
type record = list record_field

let unpack_field (b : T.binder) : T.Tac record_field = 
    let (bv, aqual) = T.inspect_binder b in
    let attr_opt = 
        match aqual with
        | T.Q_Meta_attr t -> Some t
        | _ -> None
    in
    let bvv = T.inspect_bv bv in
    // Need to make fields of bv_view 
    let open FStar.Tactics in
    { name = bvv.bv_ppname; attrs = attr_opt; typ = bvv.bv_sort; }

let rec unpack_fields (qname : list string) (ty : T.term) : T.Tac (list record_field) = 
    // type of the constructor should contain an Arrow type (there's at least one field in a record)
    match T.inspect_ln ty with
    | T.Tv_Arrow binder comp -> begin
        let f = unpack_field binder in
        match T.inspect_comp comp with
        | T.C_Total ty2 _ -> f :: unpack_fields qname ty2
        | _ -> T.fail "Unsupported computation type"
        end
    | T.Tv_FVar fv -> begin
        // The most inner part of 'ty' should be the name of the record type
        let qname2 = T.inspect_fv fv in
        if Helpers.fv_eq qname qname2
            then []
            else T.fail ("Expected " ^ (T.implode_qn qname) ^ " got " ^ (T.implode_qn qname2))
        end
    | _ -> T.fail "Expected an arrow type"

// get_record_fields (T.top_env ()) (T.explode_qn (`%r))
let get_record (env : T.env) (qname : list string) : T.Tac record = 
    match T.lookup_typ env qname with
    | Some s -> begin
        match T.inspect_sigelt s with
        | T.Sg_Inductive _ _ _ _ cts -> begin
            if List.Tot.length cts = 1
                then unpack_fields qname (snd (List.Tot.hd cts))
                else T.fail "Expected record, got inductive with more than one constructor"
            end
        | _ -> T.fail ("Expected inductive, got " ^ (T.term_to_string (T.pack (T.Tv_FVar (T.pack_fv qname)))))
        end
    | None -> T.fail ("Could not find type " ^ (T.implode_qn qname))
