module Types

module SchemaDsl = JsonStar.Schema.Dsl

type lessThanOrEqualFive = x:int{x <= 5}
type lessThanFive = x:int{x < 5}
type moreThanOrEqualFive = x:int{x >= 5}
type moreThanFive = x:int{x > 5}

type nat2 = x:int{JsonStar.Schema.Dsl.minimum x 0}
type minimum3 = x:int{JsonStar.Schema.Dsl.minimum x 3}
type maximum8 = x:int{JsonStar.Schema.Dsl.maximum x 8}

let min5max9 = x:int{x >= 5 && x <= 9}
let min5max9dsl = x:int{JsonStar.Schema.Dsl.minimum x 5 && JsonStar.Schema.Dsl.maximum x 9}

type string_max5 = s:string{String.length s <= 5}
type string_min3max5 = s:string{String.length s >= 3 && String.length s <= 5}

type string_max5_dsl = s:string{JsonStar.Schema.Dsl.maxLength s 5}
type string_min3max5_dsl = s:string{JsonStar.Schema.Dsl.minLength s 3 && JsonStar.Schema.Dsl.maxLength s 5}

type enum_onetwothree = 
    | One
    | Two
    | Three
    
type enum_onetwoonly = x:enum_onetwothree{ SchemaDsl.allow x [ One?; Two?; ]}
type enum_withouttwo = x:enum_onetwothree{ SchemaDsl.disallow x [ Two?; ]}

type enum_onetwoonly_withoutone = x:enum_onetwoonly{ SchemaDsl.disallow x [ One?;] }

type record_simple = 
    {
        field_string : string;
        field_int : int;
    }


//type record_simple_refinements = 
//    {
//        field_string_max5 : s:string{JsonStar.Schema.Dsl.maxLength s 5};
//        field_int_min5max9 : x:int{x >= 5 && x <= 9};
//        field_enum_enum_onetwoonly_withoutone : x:enum_onetwoonly{ SchemaDsl.disallow x [ One?;] };
//    }