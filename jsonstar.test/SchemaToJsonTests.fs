module SchemaToJsonTests

open NUnit.Framework
open JsonStar_Json
open JsonStar_Schema

[<TestFixture>]
type SchemaToJsonTests() = 
    
    [<Test>]
    member this.SimpleJson() =
        let s : schema = 
            { 
                _id = FStar_Pervasives_Native.None; 
                _schema = FStar_Pervasives_Native.None; 
                _type = JsonStar_Schema.String { 
                            minLength = FStar_Pervasives_Native.None; 
                            maxLength = FStar_Pervasives_Native.Some (Prims.of_int 5); 
                            pattern = FStar_Pervasives_Native.None; 
                            reference = FStar_Pervasives_Native.None; 
                        };
                description = FStar_Pervasives_Native.None;
                title = FStar_Pervasives_Native.None;
                _default = FStar_Pervasives_Native.None;
                readonly = FStar_Pervasives_Native.None;
                format = FStar_Pervasives_Native.None;
                definitions = [];
                //dependencies = [];
            }
        let expected_schema_string = """{ "maxLength" : "5", "type" : "string" }"""
        let schema_string =
            s
            |> JsonStar_Schema.toJson
            |> JsonStar_PrettyPrint.stringify
        
        printfn "%s" schema_string
        Assert.AreEqual(expected_schema_string, schema_string)

    [<Test>]
    member this.StringSchemaGeneratedWithTactics() =
        let expected_schema_string = """{ "type" : "string" }"""
        let schema_string = 
            JsonStar_Schema_Generation_Api.string_s
            |> JsonStar_Schema.toJson
            |> JsonStar_PrettyPrint.stringify

        printfn "%s" schema_string
        Assert.AreEqual(expected_schema_string, schema_string)