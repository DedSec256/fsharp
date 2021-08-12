#if INTERACTIVE
#r "../../artifacts/bin/fcs/net461/FSharp.Compiler.Service.dll" // note, build FSharp.Compiler.Service.Tests.fsproj to generate this, this DLL has a public API so can be used from F# Interactive
#r "../../artifacts/bin/fcs/net461/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module Tests.XmlDoc
#endif

open FSharp.Compiler.Service.Tests.Common
open FSharp.Compiler.Symbols
open FsUnit
open NUnit.Framework

let checkXml symbolName docs checkResults =
    let symbol = findSymbolByName symbolName checkResults
    let xmlDoc =
        match symbol with
        | :? FSharpMemberOrFunctionOrValue as v -> v.XmlDoc
        | :? FSharpEntity as t -> t.XmlDoc
        | :? FSharpUnionCase as u -> u.XmlDoc
        | _ -> failwith $"unexpected symbol type {symbol.GetType()}"
    match xmlDoc with
    | FSharpXmlDoc.FromXmlText t -> t.UnprocessedLines |> shouldEqual docs
    | _ -> failwith "wrong XmlDoc kind"

let checkXmls data checkResults =
    for symbolName, docs in data do checkXml symbolName docs checkResults

[<Test>]
let ``Simple type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
///A
type A = class end
"""
    checkResults
    |> checkXml "A" [|"A"|]

[<Test>]
let ``Multiline type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
///A

///B
type A = class end
"""
    checkResults
    |> checkXml "A" [|"A"; "B"|]

[<Test>]
let ``Separated type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
///A
()
///B
type A = class end
"""
    checkResults
    |> checkXml "A" [|"B"|]

[<Test>]
let ``Separated by simple comment type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
///A
// Simple comment delimiter
///B
type A = class end
"""
    checkResults
    |> checkXml "A" [|"B"|]

[<Test>]
let ``Separated by multiline comment type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
///A
(* Multiline comment
delimiter *)
///B
type A = class end
"""
    checkResults
    |> checkXml "A" [|"B"|]

[<Test>]
let ``Separated by star type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
///A
(*)
///B
type A = class end
"""
    checkResults
    |> checkXml "A" [|"B"|]

[<Test>]
let Test2() =
    let _, checkResults = getParseAndCheckResults """
///A
1 + 1
///B
let f x = ()
"""
    checkResults
    |> checkXml "f" [|"B"|]

[<Test>]
let Test2123() =
    let _, checkResults = getParseAndCheckResults """
let _ =
    ///A
    1 + 1
    ///B
    let x = ()
    ()
"""
    checkResults
    |> checkXml "x" [|"B"|]

[<Test>]
let ``And type xml doc``() =
    let _, checkResults = getParseAndCheckResults """
type A = class end
///B
and B = class end
"""
    checkResults
    |> checkXml "B" [|"B"|]

[<Test>]
let Test3() =
    let _, checkResults = getParseAndCheckResults """
type A = class end
and ///B
    B = class end
"""
    checkResults
    |> checkXml "B" [|"B"|]

[<Test>]
let Test31() =
    let _, checkResults = getParseAndCheckResults """
type A = class end
///B1
and ///B2
    B = class end
"""
    checkResults
    |> checkXml "B" [|"B1"|]


[<Test>]
let Test4() =
    let _, checkResults = getParseAndCheckResults """
type A =
    ///One
    One
    ///Two
    | Two
"""
    checkResults
    |> checkXmls [
        "One", [|"One"|]
        "Two", [|"Two"|]
    ]


[<Test>]
let Test41() =
    let _, checkResults = getParseAndCheckResults """
type A =
    member x.A() = ///B
        ()

    member x.B() = ()
"""
    checkResults
    |> checkXml "B" [||]

[<Test>]
let Test42() =
    let _, checkResults = getParseAndCheckResults """
type A =
    member x.A() = ///B1
        ()

    ///B2
    ///B3
    member x.B() = ()
"""
    checkResults
    |> checkXml "B" [|"B2"; "B3"|]

[<Test>]
let Test426() =
    let _, checkResults = getParseAndCheckResults """
type A =
    ///B1
    ///B2
    [<NotNull>]
    ///B3
    member x.B() = ()
"""
    checkResults
    |> checkXml "B" [|"B1"; "B2"|]

[<Test>]
let Test421() =
    let _, checkResults = getParseAndCheckResults """
type A =
    ///B1
    member
           ///B2
           private x.B() = ()
"""
    checkResults
    |> checkXml "B" [|"B1"|]

[<Test>]
let Test141() =
    let _, checkResults = getParseAndCheckResults """
///A1
///A2
[<NotNull>]
///B
type A = class end
"""
    checkResults
    |> checkXml "A" [|"A1"; "A2"|]

[<Test>]
let Test142() =
    let _, checkResults = getParseAndCheckResults """
type A = class end
and
    ///B1
    [<NotNull>]
    ///B2
    B = class end
"""
    checkResults
    |> checkXml "B" [|"B1"|]


[<Test>]
let Test143() =
    let _, checkResults = getParseAndCheckResults """
[<NotNull>]
///A
type A = class end
"""
    checkResults
    |> checkXml "A" [||]

[<Test>]
let Test144() =
    let _, checkResults = getParseAndCheckResults """
type A =
    ///M1
    ///M2
    abstract member M: unit
"""
    checkResults
    |> checkXml "get_M" [|"M1"; "M2"|]

[<Test>]
let Test145() =
    let _, checkResults = getParseAndCheckResults """
type A =
    ///M1
    [<NotNull>]
    ///M2
    abstract member M: unit
"""
    checkResults
    |> checkXml "get_M" [|"M1"|]

[<Test>]
let ``Property accessors xml doc``() =
    let _, checkResults = getParseAndCheckResults """
type B =
    ///A1
    ///A2
    member ///A3
           x.A
                ///GET
                with get () = 5
                ///SET
                and set (_: int) = ()

    member x.C = x.set_A(4)
"""

    checkResults
    |> checkXmls [
        "get_A", [|"A1"; "A2"|]
        "set_A", [|"A1"; "A2"|]
    ]
