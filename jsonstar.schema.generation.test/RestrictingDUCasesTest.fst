module RestrictingDUCasesTest

open JsonStar.Schema.Dsl

module AST = JsonStar.Schema.Ast.Types

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

type custom_record = 
	{
		foo_field : enum_required #foo_enum [ FooTwo?; FooFour?; ]; 
		yes_no_option_field : option yes_no;
		bar_field : enum_forbidden #bar_enum [ BarOne?; BarFour?; ];
		non_negative_field : non_negative;
		negative_field : negative;
	}

type dep_example = 
	| Standard : dep_example
	| Custom : v:custom_record -> dep_example

type option1_record = 
	{
		option1_enum : foo_enum;
		[@@@ AST.defaultValue "Custom"]
		option1_enum_dep : dep_example;
	}

type option2_record =
	{
		option2_enum : bar_enum;
		[@@@ AST.defaultValue "Standard"]
		option2_enum_dep : dep_example;
		option2_optional_field : option non_negative;
	}

type option3_record =
	{
		option3_field1 : date;
		option3_field2 : non_negative;
		option3_field3 : option negative;
	}

type subschema_selection = 
	| Option1 : v:option1_record -> subschema_selection
	| Option2 : v:option2_record -> subschema_selection
	| Option3 : v:option3_record -> subschema_selection
	| Option4 : v:option3_record -> subschema_selection

type record_with_du_dependency = 
	{
		[@@@ AST.description "Record id"]
		id : string;
		dep_field : subschema_selection;
	}

// TODO: Add support for restricting DUs
type restricted_record_with_du_dependency = required #record_with_du_dependency (fun x -> x.dep_field) [ Option2?; ]

// TODO: Add some examples
let ex1 : dep_example = Standard
let ex2 : dep_example = 
	Custom 
		({
			foo_field = FooTwo;
			yes_no_option_field = None;
			bar_field = BarThree;
			non_negative_field = 5;
			negative_field = -3;
		})


let schema_complex_dep_example =
    let s : JsonStar.Schema.schema = (FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`restricted_record_with_du_dependency))) in
    JsonStar.PrettyPrint.stringify (JsonStar.Schema.toJson s)