module JsonStar.Schema.Generation.Api

open JsonStar.Schema

module T = FStar.Tactics 
module SchemaGen = JsonStar.Schema.Generation
module SchemaDsl = JsonStar.Schema.Dsl

let string_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`string))

let nat_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`nat))

type nat2 = x:int{SchemaDsl.minimum x 0}
let nat2_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`nat2))

let min5max9 = x:int{x >= 5 && x <= 9}
let min5max9_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`min5max9))

type string_max5 = s:string{SchemaDsl.maxLength s 5}
let string_max5_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`string_max5))

type string_min3max5 = s:string{SchemaDsl.minLength s 3 && SchemaDsl.maxLength s 5}
let string_min3max5_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`string_min3max5))

type string_pat = s:string{SchemaDsl.pattern s "YYYY-MM-DD"}
let string_pat_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`string_pat))

type enum_onetwothree = 
    | One
    | Two
    | Three
let enum_onetwothree_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`enum_onetwothree))

//let x : enum_onetwothree = T.synth_by_tactic (fun () -> SchemaGen.print_term T.Goal (`One))

type enum_onetwoonly = x:enum_onetwothree{ SchemaDsl.allow x [ One?; Two?; ]}
type enum_withoutthree = x:enum_onetwothree{ SchemaDsl.disallow x [ Two?; ]}

let enum_onetwoonly_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`enum_onetwoonly))
let enum_withoutthree_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`enum_withoutthree))