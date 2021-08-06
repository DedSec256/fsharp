module Tests.XmlDoc

open FSharp.Compiler.Service.Tests.Common
open FSharp.Compiler.Symbols
open FsUnit
open NUnit.Framework

type XmlDoc() =

    let compareXml (symbol: FSharpSymbol) docs =
        let xmlDoc =
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as v -> v.XmlDoc
            | :? FSharpEntity as t -> t.XmlDoc
            | _ -> failwith "wrong symbol!"
        match xmlDoc with
        | FSharpXmlDoc.FromXmlText t -> t.UnprocessedLines |> shouldEqual docs
        | _ -> failwith "wrong XmlDoc kind"

    [<Test>]
    [<TestCase("s", "A")>]
    member x.Test(symbol, xml) =
        let _, checkResults = getParseAndCheckResults """
///A
type A = class end
"""
        let symbol = findSymbolByName symbol checkResults
        compareXml symbol [|xml|]
