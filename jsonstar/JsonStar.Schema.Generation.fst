(*
The idea of this module is to provide a functionality that allows 
to translate a F* type into a json-schema representation.

It can be used in a following way:

let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`YOUR_TYPE))
e.g. 
let s : schema = T.synth_by_tactic (fun () -> gen_schema T.Goal (`string))
*)
module JsonStar.Schema.Generation

module T = FStar.Tactics

/// Schema generation tactic
/// @ignore_synonyms - replace type abbreviations with its definition
/// @typ - term representation of a type that we want to produce json-schema for
let gen_schema (pol : T.guard_policy) (t : T.term) : T.Tac unit = 
    let ast = JsonStar.Schema.Generation.Reader.fromTerm (T.top_env()) t in
    let s = JsonStar.Schema.Ast.ToJsonSchema.toJsonSchema ast in
    T.exact_guard (quote s)
