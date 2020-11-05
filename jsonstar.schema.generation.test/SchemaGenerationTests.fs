namespace jsonstar.fstar.generation.test

open NUnit.Framework
open JsonStar_Json

[<TestFixture>]
type SchemaGenerationTests() = 
    
    [<Test>]
    member this.StringSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "type" : "string" }"""
        
        printfn "%s" SchemaExamples.schema_string
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_string)

    [<Test>]
    member this.IntSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "type" : "integer" }"""
        
        printfn "%s" SchemaExamples.schema_int
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_int)

    [<Test>]
    member this.BoolSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "type" : "boolean" }"""
        
        printfn "%s" SchemaExamples.schema_bool
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_bool)

    [<Test>]
    member this.NatSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "minimum" : "0", "type" : "integer" }"""
          
        printfn "%s" SchemaExamples.schema_nat
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_nat)

    [<Test>]
    member this.MoreThanOrEqualFiveSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "minimum" : "5", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_moreThanOrEqualFive)
    
    [<Test>]
    member this.MoreThanFiveSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "minimum" : "6", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_moreThanFive)

    [<Test>]
    member this.LessThanOrEqualFiveSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "maximum" : "5", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_lessThanOrEqualFive)

    [<Test>]
    member this.LessThanFiveSchema() =
        let expected_schema_string = """{ "definitions" : {  }, "maximum" : "4", "type" : "integer" }"""
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_lessThanFive)