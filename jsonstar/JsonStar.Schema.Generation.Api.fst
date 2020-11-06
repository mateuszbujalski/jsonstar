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