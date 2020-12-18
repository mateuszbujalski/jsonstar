﻿namespace jsonstar.fstar.generation.test

open NUnit.Framework
open JsonStar_Json

[<TestFixture>]
type SchemaGenerationTests() = 
    
    [<Test>]
    member this.StringSchema() =
        let expected_schema_string = """{ "type" : "string" }"""
        
        printfn "%s" SchemaExamples.schema_string
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_string)

    [<Test>]
    member this.IntSchema() =
        let expected_schema_string = """{ "type" : "integer" }"""
        
        printfn "%s" SchemaExamples.schema_int
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_int)

    [<Test>]
    member this.BoolSchema() =
        let expected_schema_string = """{ "type" : "boolean" }"""
        
        printfn "%s" SchemaExamples.schema_bool
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_bool)

    [<Test>]
    member this.NatSchema() =
        let expected_schema_string = """{ "minimum" : "0", "type" : "integer" }"""
          
        printfn "%s" SchemaExamples.schema_nat
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_nat)

    [<Test>]
    member this.MoreThanOrEqualFiveSchema() =
        let expected_schema_string = """{ "minimum" : "5", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_moreThanOrEqualFive)
    
    [<Test>]
    member this.MoreThanFiveSchema() =
        let expected_schema_string = """{ "minimum" : "6", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_moreThanFive)

    [<Test>]
    member this.LessThanOrEqualFiveSchema() =
        let expected_schema_string = """{ "maximum" : "5", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_lessThanOrEqualFive)

    [<Test>]
    member this.LessThanFiveSchema() =
        let expected_schema_string = """{ "maximum" : "4", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_lessThanFive)

    [<Test>]
    member this.nat2Schema() =
        let expected_schema_string = """{ "minimum" : "0", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_nat2)

    [<Test>]
    member this.minimum3Schema() =
        let expected_schema_string = """{ "minimum" : "3", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_minimum3)

    [<Test>]
    member this.maximum8Schema() =
        let expected_schema_string = """{ "maximum" : "8", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_maximum8)

    [<Test>]
    member this.min5max9Schema() =
        let expected_schema_string = """{ "minimum" : "5", "maximum" : "9", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_min5max9)

    [<Test>]
    member this.min5max9dslSchema() =
        let expected_schema_string = """{ "minimum" : "5", "maximum" : "9", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_min5max9dsl)

    [<Test>]
    member this.string_max5Schema() =
        let expected_schema_string = """{ "maxLength" : "5", "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_string_max5)

    [<Test>]
    member this.string_min3max5Schema() =
        let expected_schema_string = """{ "maxLength" : "5", "minLength" : "3", "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_string_min3max5)

    [<Test>]
    member this.string_max5dslSchema() =
        let expected_schema_string = """{ "maxLength" : "5", "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_string_max5_dsl)

    [<Test>]
    member this.string_min3max5dslSchema() =
        let expected_schema_string = """{ "maxLength" : "5", "minLength" : "3", "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_string_min3max5_dsl)

    [<Test>]
    member this.enum_onetwothreeSchema() =
        let expected_schema_string = """{ "enum" : [ "One", "Two", "Three" ], "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_enum_onetwothree)

    [<Test>]
    member this.enum_onetwoonlySchema() =
        let expected_schema_string = """{ "enum" : [ "One", "Two" ], "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_enum_onetwoonly)

    [<Test>]
    member this.enum_withouttwoSchema() =
        let expected_schema_string = """{ "enum" : [ "One", "Three" ], "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_enum_withouttwo)

    [<Test>]
    member this.enum_onetwoonly_withoutoneSchema() =
        let expected_schema_string = """{ "enum" : [ "Two" ], "type" : "string" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_enum_onetwoonly_withoutone)

    [<Test>]
    member this.record_simpleSchema() =
        let expected_schema_string = """{ "properties" : { "field_string" : { "type" : "string" }, "field_int" : { "type" : "integer" } }, "additionalProperties" : "False", "required" : [ "field_string", "field_int" ], "type" : "object" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_record_simple)

    [<Test>]
    member this.record_simple_refinementsSchema() =
        let expected_schema_string = """{ "properties" : { "field_string_max5" : { "maxLength" : "5", "type" : "string" }, "field_int_min5max9" : { "minimum" : "5", "maximum" : "9", "type" : "integer" }, "field_enum_enum_onetwoonly_withoutone" : { "enum" : [ "Two" ], "type" : "string" } }, "additionalProperties" : "False", "required" : [ "field_string_max5", "field_int_min5max9", "field_enum_enum_onetwoonly_withoutone" ], "type" : "object" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_record_simple_refinements)

    [<Test>]
    member this.record_simple_refinements_type_abbrevSchema() =
        let expected_schema_string = """{ "properties" : { "field_string_max5_ab" : { "maxLength" : "5", "type" : "string" }, "field_int_min5max9_ab" : { "minimum" : "5", "maximum" : "9", "type" : "integer" }, "field_enum_enum_onetwoonly_withoutone_ab" : { "enum" : [ "Two" ], "type" : "string" } }, "additionalProperties" : "False", "required" : [ "field_string_max5_ab", "field_int_min5max9_ab", "field_enum_enum_onetwoonly_withoutone_ab" ], "type" : "object" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_record_simple_refinements_type_abbrev)
    
    [<Test>]
    member this.record_with_optional_fieldSchema() =
        let expected_schema_string = """{ "properties" : { "field_required" : { "minimum" : "0", "type" : "integer" }, "field_optional" : { "minimum" : "5", "maximum" : "9", "type" : "integer" } }, "additionalProperties" : "False", "required" : [ "field_required" ], "type" : "object" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_record_with_optional_field)

    [<Test>]
    member this.record_with_depSchema() =
        let expected_schema_string = """{ "properties" : { "foo_type" : { "enum" : [ "DepOne", "DepTwo" ], "type" : "string" } }, "additionalProperties" : "False", "required" : [ "foo_type" ], "dependencies" : { "foo_type" : { "oneOf" : [ { "foo_type" : { "enum" : [ "DepOne" ] }, "properties" : { "fielda" : { "type" : "integer" } }, "additionalProperties" : "False", "required" : [ "fielda" ], "type" : "object" }, { "foo_type" : { "enum" : [ "DepTwo" ] }, "properties" : { "fieldb" : { "type" : "string" } }, "additionalProperties" : "False", "required" : [ "fieldb" ], "type" : "object" } ] } }, "type" : "object" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_record_with_dep)
