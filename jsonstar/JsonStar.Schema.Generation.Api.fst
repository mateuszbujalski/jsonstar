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

type enum_onetwoonly_withoutone = x:enum_onetwoonly{ SchemaDsl.disallow x [ One?;] }

type record_simple = 
    {
        field_string : string;
        field_int : int;
    }

let record_simple_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`record_simple))

type record_simple_refinements_typenames = 
    {
        field_string_max5 : string_max5;
        field_int_min5max9 : min5max9;
        field_enum_enum_onetwoonly_withoutone : enum_onetwoonly_withoutone;
    }
let record_simple_refinements_typenames_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`record_simple_refinements_typenames))

type record_with_optional_field = 
    {
        field_required : nat2;
        field_optional : option min5max9;
    }
let record_with_optional_field_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`record_with_optional_field))

type foo_one = { fielda : int; }
type foo_two = { fieldb : string; }
type du_dep =
    | DepOne : v:foo_one -> du_dep
    | DepTwo : v:foo_two -> du_dep
type record_with_dep = 
    {
        field_dep : du_dep;
    }
let record_with_dep_s : schema = T.synth_by_tactic (fun () -> SchemaGen.gen_schema T.Goal (`record_with_dep))