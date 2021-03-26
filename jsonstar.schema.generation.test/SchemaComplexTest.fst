module SchemaComplexTest

open JsonStar.Schema.Dsl

type date = x:string{ pattern x "YYYY-MM-DD" }

type non_negative = x:int{x >= 0}
type negative = x:int{x < 0}

type yes_no = 
	| Yes 
	| No

type foo_enum = 
	| FooOne
	| FooTwo
	| FooThree
	| FooFour

type bar_enum = 
	| BarOne
	| BarTwo
	| BarThree
	| BarFour

type standard_record = 
	{
		dum : string;
	}

type custom_record = 
	{
		foo_field : allowed #foo_enum [ FooTwo?; FooFour?; ]; 
		yes_no_option_field : option yes_no;
		bar_field : disallowed #bar_enum [ BarOne?; BarFour?; ];
		non_negative_field : non_negative;
		negative_field : negative;
	}

type dep_example = 
	| Standard : v:standard_record -> dep_example // TODO: Drop standard_record here
	| Custom : v:custom_record -> dep_example

let ex1 : dep_example = Standard ({ dum = "aaaa"; })
let ex2 : dep_example = 
	Custom 
		({
			foo_field = FooTwo;
			yes_no_option_field = None;
			bar_field = BarThree;
			non_negative_field = 5;
			negative_field = -3;
		})

// TODO: Add a test for allowed / disallowed with DUs instead of enums

// TODO: Fix schema generation for "allowed / disallowed"
//let schema_complex_dep_example =
//    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`dep_example))) in
//    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)