module SchemaExamples

type lessThanOrEqualFive = x:int{x <= 5}
type lessThanFive = x:int{x < 5}
type moreThanOrEqualFive = x:int{x >= 5}
type moreThanFive = x:int{x > 5}

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

let schema_lessThanOrEqualFive = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`lessThanOrEqualFive))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_lessThanFive = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`lessThanFive))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_moreThanOrEqualFive = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`moreThanOrEqualFive))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_moreThanFive = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`moreThanFive))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)