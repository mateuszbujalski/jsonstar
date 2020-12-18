(*
The idea of this module is to provide a functionality that allows 
to translate a F* type into a json-schema representation.

It can be used in a following way:

let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`YOUR_TYPE))
e.g. 
let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`string))
*)
module JsonStar.Schema.Generation

// TODO: Create a DSL for describing supported refinements? 
//       Or maybe it would be better to parameterize gen_schema with a list of 
//       "converters" for handling refinements in types? 
//       I don't think we can handle arbitrary refinements for schema generation.

open FStar.String

module T = FStar.Tactics
module L = FStar.List.Tot

open JsonStar.Schema
open JsonStar.Json
open JsonStar.PrettyPrint

module Helpers = JsonStar.Schema.Generation.Helpers
module Primitive = JsonStar.Schema.Generation.Primitive
module Enum = JsonStar.Schema.Generation.Enum
module Refinement = JsonStar.Schema.Generation.Refinement
module Recognizers = JsonStar.Schema.Generation.Recognizers

open FStar.Reflection.Data

let min_opt (x : option number) (y : option number) : option number = 
    match x, y with
    | Some (Int x), Some (Int y) -> Some (Int (FStar.Math.Lib.min x y))
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None

let max_opt (x : option number) (y : option number) : option number = 
    match x, y with
    | Some (Int x), Some (Int y) -> Some (Int (FStar.Math.Lib.max x y))
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None

let with_default (#a:Type) (def : a) (x : option a) : z:option a{Some? z} = 
    match x with
    | Some a -> Some a
    | None -> Some def

let rec refinementToSchema (t : T.term) : T.Tac schema = 
    match T.inspect t with
    | T.Tv_Refine b phi ->
        let b = T.inspect_bv b in
        let baseTerm : T.term = b.bv_sort in
        // TODO: Handle refinements
        let baseSchema = genSchema baseTerm in
        let refinedSchema = 
            match baseSchema._type with
            | String opt -> begin 
                let refs = Refinement.stringRefinementsFromTerm phi in
                let nopt = 
                    T.fold_left 
                        (fun opt ref ->
                            match ref with
                            | Refinement.MaxLength v -> { opt with maxLength = with_default v (Option.mapTot (fun x -> JsonStar.Math.min_nat x v) opt.maxLength) }
                            | Refinement.MinLength v -> { opt with minLength = with_default v (Option.mapTot (fun x -> JsonStar.Math.max_nat x v) opt.minLength) }
                            | Refinement.Pattern s -> { opt with pattern = Some s}) // TODO: Combine patterns for nested refinements?
                        opt 
                        refs
                in 
                { baseSchema with _type = String nopt }
                end
            | Enum vs -> begin 
                let ref = Refinement.enumRefinementFromTerm phi in
                let nvs = 
                    match ref with
                    | Refinement.Allow values -> L.filter (fun x -> L.mem x values) vs
                    | Refinement.Disallow values -> L.filter (fun x -> not (L.mem x values)) vs
                in
                { baseSchema with _type = Enum nvs }
                end
            | Integer -> baseSchema
            | Number opt -> begin 
                let refs = Refinement.numberRefinementsFromTerm phi in
                let nopt = 
                    T.fold_left 
                        (fun opt ref ->
                            match ref with
                            | Refinement.Maximum v -> { opt with maximum = min_opt opt.maximum (Some (Int v)) }
                            | Refinement.Minimum v -> { opt with minimum = max_opt opt.minimum (Some (Int v)) }) 
                        opt 
                        refs
                in 
                { baseSchema with _type = Number nopt }
                end
            | Boolean -> baseSchema
            // TODO: all below
            | Object _ _ _ -> baseSchema
            | Array _ _ -> baseSchema
            | Reference _ -> baseSchema
            | OneOf _ -> baseSchema
        in
        refinedSchema
    | _ -> Helpers.tfail ("Expected " ^ (T.term_to_string t) ^ " to be refinement")

and recordToSchema (r : JsonStar.Schema.Generation.Record.record) : T.Tac schema =  
    let open JsonStar.Schema.Generation.Record in
    let fields_schema = T.map (fun rf -> 
        let is_required, typ = 
            match rf.typ with
            | Optional ft -> false, ft
            | Required ft -> true, ft
        in
        let typ = Helpers.drop_synonym (T.top_env()) typ in
        // Recognize if typ is a DU
        // If it is, then we need to extract it's type name and constructor names and create an extra enum
        // field in the schema. Then, we need to introduce a dependency on the enum field with the schema 
        // depending on the value of the enum
        if JsonStar.Schema.Generation.Dependencies.isOneOfDU typ
            then 
                let open JsonStar.Schema.Generation.Dependencies in
                let dep = get_dependency (T.cur_env()) typ in
                match dep with
                | OneOf e subschemas -> begin
                    // TODO: Create enum field and return an extra dependency for a record
                    // NOTE: The autogenerated enum is marked as 'required'
                    e.name, true, None, (Enum.mkenum e.fields), (Some subschemas)
                    end
            else rf.name, is_required, rf.attrs, (genSchema typ), None
        ) r 
    in
    // TODO: Handle DUs / dependencies
    // TODO: Use attributes to enhance schema
    // TODO: Do not inline schemas for fields, use definitions & references
    let props = T.map (fun (name, _, _, s, _) -> name, s) fields_schema in
    // NOTE: We don't support dependency on two enum fields - perhaps we should?
    let deps = 
        T.filter_map 
            (fun (name, _, _, _, deps_opt) -> 
                let open JsonStar.Schema.Generation.Dependencies in
                match deps_opt with
                | Some deps -> Some (name, T.map (fun dep -> (dep.value, (genSchema dep.subschema))) deps)
                | None -> None
            ) fields_schema 
    in
    let req = T.filter_map (fun (name, isReq, _, _, _) -> if isReq then Some name else None) fields_schema in
    mkSchemaEmpty (Object props deps ({ required = Some req; additionalProperties = None; }))

// Entry point for schema generation
and genSchema (t : T.term) : T.Tac schema = 
    let tt : Recognizers.term_type = Recognizers.term_to_type t in
    match tt with
    | Recognizers.Primitive t -> Primitive.toSchema t
    | Recognizers.Refinement t -> refinementToSchema t
    | Recognizers.Enum fv -> Enum.toSchema fv
    | Recognizers.Record r -> recordToSchema r
    | Recognizers.Unrecognized t -> Helpers.tfail ("Unsupported type while generating schema:\n" ^ (T.term_to_string t) ^ "\n")

/// Schema generation tactic
/// @ignore_synonyms - replace type abbreviations with its definition
/// @typ - term representation of a type that we want to produce json-schema for
let gen_schema' (ignore_synonyms : bool) (typ: T.term) : T.Tac T.term =
    let t = 
        if ignore_synonyms
            then Helpers.drop_synonym (T.top_env()) typ
            else fst (Helpers.app_head_tail typ)
    in
    
    Helpers.printAst t;
    //Helpers.printAst (T.norm_term [] t);

    let s = genSchema t in
    quote s

let gen_schema (pol: T.guard_policy) (t:T.term) : T.Tac unit =
    let s = gen_schema' true t in
    // Somehow, this line causes the tactic to get stuck when actually trying to print it.
    // Either with T.print, or T.debug with debug flag is enabled. 
    let s_string : string = JsonStar.PrettyPrint.stringify (toJson (T.unquote s)) in
    // F* prints the stringified result when run with 
    // [--debug JsonStar.Schema.Generation --debug_level Tac] flags. 
    // It's rather verbose. For now we just use print.
    //T.print s_string;
    T.debug ("Schema produced for (" ^ (T.term_to_string t) ^ "):\n" ^ s_string ^ "\n");
    T.exact_guard s


let print_term (pol: T.guard_policy) (t:T.term) : T.Tac unit = 
    T.print "AAAAAAAAAAAAAAAAAAAAAAAAAAA";
    Helpers.printAst t;
    T.exact_guard t