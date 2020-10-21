module SchemaExamples

let schema_string : string = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_int : string = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`int))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_bool : string = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`bool))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_nat : string = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`nat))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)