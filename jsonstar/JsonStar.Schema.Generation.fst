(*
The idea of this module is to provide a functionality that allows 
to translate a F* type into a json-schema representation.

It can be used in a following way:

let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`YOUR_TYPE))
e.g. 
let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`string))
*)
module JsonStar.Schema.Generation

// open FStar.String

module T = FStar.Tactics
// module L = FStar.List.Tot

// open JsonStar.Schema
// open JsonStar.Json
// open JsonStar.PrettyPrint

// open FStar.Reflection.Data

// /// Schema generation tactic
// /// @ignore_synonyms - replace type abbreviations with its definition
// /// @typ - term representation of a type that we want to produce json-schema for
// let gen_schema' (ignore_synonyms : bool) (typ: T.term) : T.Tac T.term =
//     let t = 
//         if ignore_synonyms
//             then Helpers.drop_synonym (T.top_env()) typ
//             else fst (Helpers.app_head_tail typ)
//     in
    
//     Helpers.printAst t;
//     //Helpers.printAst (T.norm_term [] t);

//     let s = genSchema t in
//     quote s

// let gen_schema (pol: T.guard_policy) (t:T.term) : T.Tac unit =
//     let s = gen_schema' true t in
//     // Somehow, this line causes the tactic to get stuck when actually trying to print it.
//     // Either with T.print, or T.debug with debug flag is enabled. 
//     let s_string : string = JsonStar.PrettyPrint.stringify (toJson (T.unquote s)) in
//     // F* prints the stringified result when run with 
//     // [--debug JsonStar.Schema.Generation --debug_level Tac] flags. 
//     // It's rather verbose. For now we just use print.
//     //T.print s_string;
//     T.debug ("Schema produced for (" ^ (T.term_to_string t) ^ "):\n" ^ s_string ^ "\n");
//     T.exact_guard s

/// Schema generation tactic
/// @ignore_synonyms - replace type abbreviations with its definition
/// @typ - term representation of a type that we want to produce json-schema for
let gen_schema (pol : T.guard_policy) (t : T.term) : T.Tac unit = 
    let ast = JsonStar.Schema.Generation.Reader.fromTerm (T.top_env()) t in
    let s = JsonStar.Schema.Ast.ToJsonSchema.toJsonSchema ast in
    T.exact_guard (quote s)

let print_term (pol: T.guard_policy) (t:T.term) : T.Tac unit = 
    T.print "AAAAAAAAAAAAAAAAAAAAAAAAAAA";
    JsonStar.Schema.Generation.Helpers.printAst t;
    T.exact_guard t