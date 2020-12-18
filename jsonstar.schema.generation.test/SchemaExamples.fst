module SchemaExamples

open Types

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

let schema_string_max5 =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string_max5))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_string_min3max5 =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string_min3max5))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_string_max5_dsl =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string_max5_dsl))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_string_min3max5_dsl =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string_min3max5_dsl))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_enum_onetwothree =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`enum_onetwothree))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_enum_onetwoonly =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`enum_onetwoonly))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_enum_withouttwo =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`enum_withouttwo))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_enum_onetwoonly_withoutone =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`enum_onetwoonly_withoutone))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_record_simple =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`record_simple))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_record_simple_refinements =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`record_simple_refinements))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_record_simple_refinements_type_abbrev =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`record_simple_refinements_type_abbrev))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)
   
let schema_record_with_optional_field =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`record_with_optional_field))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)

let schema_record_with_dep =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`record_with_dep))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)
