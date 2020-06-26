module ParseTests

open NUnit.Framework
open JsonStar_Json

[<TestFixture>]
type ParseTests() = 
    
    let loadFile (path : string) : string = 
        System.IO.File.ReadAllText(path)

    [<Test>]
    member this.SimpleJson() =
        let path = System.IO.Path.Combine(TestContext.CurrentContext.TestDirectory, @".\data\SimpleJson.json")
        printfn "%s" path
        printfn "%s" System.Environment.CurrentDirectory
        let content = loadFile path
        let j = JsonStar_Parser.parse content
        let j_expected = JObject [ "test", JString "testvalue" ]

        let s = JsonStar_PrettyPrint.stringify j
        let s_expected = "{ \"test\" : \"testvalue\" }"

        Assert.AreEqual(j_expected, j)
        Assert.AreEqual(s_expected, s)