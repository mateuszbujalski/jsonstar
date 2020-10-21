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
        let expected_schema_string = """{ "definitions" : {  }, "type" : "integer", "minimum":0 }"""
          
        printfn "%s" SchemaExamples.schema_nat
        Assert.AreEqual(expected_schema_string, SchemaExamples.schema_nat)
    