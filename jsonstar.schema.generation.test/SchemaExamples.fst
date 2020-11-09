module SchemaExamples

type lessThanOrEqualFive = x:int{x <= 5}
type lessThanFive = x:int{x < 5}
type moreThanOrEqualFive = x:int{x >= 5}
type moreThanFive = x:int{x > 5}

type nat2 = x:int{JsonStar.Schema.Dsl.minimum x 0}
type minimum3 = x:int{JsonStar.Schema.Dsl.minimum x 3}
type maximum8 = x:int{JsonStar.Schema.Dsl.maximum x 8}

let min5max9 = x:int{x >= 5 && x <= 9}
let min5max9dsl = x:int{JsonStar.Schema.Dsl.minimum x 5 && JsonStar.Schema.Dsl.maximum x 9}

type string_max5 = s:string{JsonStar.Schema.Dsl.maxLength s 5}
type string_min3max5 = s:string{JsonStar.Schema.Dsl.minLength s 3 && JsonStar.Schema.Dsl.maxLength s 5}

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

let schema_nat2 = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`nat2))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_minimum3 = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`minimum3))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_maximum8 = 
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`maximum8))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_min5max9 =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`min5max9))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_min5max9dsl =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`min5max9dsl))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_string_max5_dsl =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string_max5))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_string_min3max5_dsl =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string_min3max5))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)
