(*
The idea of this module is to provide a functionality that allows 
to translate a F* type into a json-schema representation.

It can be used in a following way:

let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`YOUR_TYPE))
e.g. 
let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`string))
*)
module JsonStar.Schema.Generation

// TODO: Fix F# compilation (requires FStar.Tactics in ulib?)
// TODO: Create a DSL for describing supported refinements? 
//       Or maybe it would be better to parameterize gen_schema with a list of 
//       "converters" for handling refinements in types? 
//       I don't think we can handle arbitrary refinements for schema generation.

open FStar.String
open FStar.Tactics

module T = FStar.Tactics
module L = FStar.List.Tot

open JsonStar.Schema
open JsonStar.Json
open JsonStar.PrettyPrint

let tfail (#a: Type) (s:string) : T.Tac a =
    T.debug ("Tactic failure: " ^ s);
    T.fail s

let rec app_head_rev_tail (t: T.term) : T.Tac (T.term * list T.argv) =
    let ins = T.inspect t in
    if T.Tv_App? ins
    then 
        let (T.Tv_App u v) = ins in
        let (x, l) = app_head_rev_tail u in
        (x, v :: l)
    else (t, [])

// Tactic that extracts a term and potentially it's arguments if it's a function application
let app_head_tail (t : T.term) : T.Tac (T.term * list T.argv) =
    let (x,l) = app_head_rev_tail t in
    (x, L.rev l)

let drop_synonym (e : env) (t : T.term) : T.Tac (T.term) =
    // delta normalization unfolds names
    norm_term_env e [delta] t

// TODO: Not implemented yet
let tryGenSchema (t : T.term) : T.Tac (option schema) = 
    //None
    Some 
        ({
            _id = None;
            _schema = None;
            _type = String (mkempty_string_options ());
            description = None;
            title = None;
            _default = None;
            definitions = [];
        })

/// Schema generation tactic
/// @ignore_synonyms - replace type abbreviations with its definition
/// @typ - term representation of a type that we want to produce json-schema for
let gen_schema' (ignore_synonyms : bool) (typ: T.term) : T.Tac (T.term) =
    let t = 
        if ignore_synonyms
            then drop_synonym (top_env()) typ
            else fst (app_head_tail typ)
    in
    let s : option schema = tryGenSchema t in

    match s with
    | Some s' -> quote s'
    | None -> tfail ("Unsupported type while generating schema:\n" ^ (term_to_string typ) ^ "\n")

let gen_schema (pol: T.guard_policy) (t:T.term) : T.Tac unit =
    let s = gen_schema' true t in
    // Somehow, this line causes the tactic to get stuck when actually trying to print it.
    // Either with T.print, or T.debug with debug flag is enabled. 
    let s_string : string = JsonStar.PrettyPrint.stringify (toJson (unquote s)) in
    // F* prints the stringified result when run with 
    // [--debug JsonStar.Schema.Generation --debug_level Tac] flags. 
    // It's rather verbose. For now we just use print.
    //T.print s_string;
    T.debug ("Schema produced for (" ^ (term_to_string t) ^ "):\n" ^ s_string ^ "\n");
    exact_guard s
