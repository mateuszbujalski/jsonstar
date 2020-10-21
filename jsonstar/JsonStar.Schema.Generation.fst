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
module Refinement = JsonStar.Schema.Generation.Refinement

open FStar.Reflection.Data

let refinementToSchema (t : T.term) : T.Tac (option schema) = 
    match T.inspect t with
    | T.Tv_Refine b phi ->
        let b = T.inspect_bv b in
        let baseTerm : T.term = b.bv_sort in
        // TODO: Handle refinements
        Primitive.toSchema baseTerm
    | _ -> Helpers.tfail ("Expected " ^ (T.term_to_string t) ^ " to be refinement")

let tryGenSchema (t : T.term) : T.Tac (option schema) = 
    let ast = FStar.Tactics.Print.term_to_ast_string t in
    T.print ast;
    
    let tv : T.term_view = T.inspect t in
    if Primitive.isPrimitive tv then Primitive.toSchema t
    else if Refinement.isRefinement tv then refinementToSchema tv
    else None

/// Schema generation tactic
/// @ignore_synonyms - replace type abbreviations with its definition
/// @typ - term representation of a type that we want to produce json-schema for
let gen_schema' (ignore_synonyms : bool) (typ: T.term) : T.Tac (T.term) =
    let t = 
        if ignore_synonyms
            then Helpers.drop_synonym (T.top_env()) typ
            else fst (Helpers.app_head_tail typ)
    in
    let s : option schema = tryGenSchema t in

    match s with
    | Some s' -> quote s'
    | None -> Helpers.tfail ("Unsupported type while generating schema:\n" ^ (T.term_to_string typ) ^ "\n")

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
