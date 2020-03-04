#if INTERACTIVE
#r "../../artifacts/bin/fcs/net461/FSharp.Compiler.Service.dll" // note, build FSharp.Compiler.Service.Tests.fsproj to generate this, this DLL has a public API so can be used from F# Interactive
#r "../../artifacts/bin/fcs/net461/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.ExtensionTypingProvider
#endif

open System
open System.IO
open FsUnit
open NUnit.Framework
open FSharp.Compiler.ExtensionTyping 
open FSharp.Compiler.Range
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.Service.Tests.Common
open FSharp.Compiler
open FSharp.Compiler.SourceCodeServices

[<Test>]
let ``Extension typing shim gets requests`` () =
    let mutable gotRequest = false
    let defaultExtensionTypingShim = Shim.ExtensionTypingProvider
    
    let extensionTypingProvider =
        { new IExtensionTypingProvider with
            member this.InstantiateTypeProvidersOfAssembly
                    (runTimeAssemblyFileName: string, 
                     ilScopeRefOfRuntimeAssembly: ILScopeRef, 
                     designTimeAssemblyNameString: string, 
                     resolutionEnvironment: ResolutionEnvironment, 
                     isInvalidationSupported: bool, 
                     isInteractive: bool, 
                     systemRuntimeContainsType: string -> bool, 
                     systemRuntimeAssemblyVersion: System.Version, 
                     compilerToolPaths: string list,
                     m: range) =
                gotRequest <- true
                []
        }
        
    Shim.ExtensionTypingProvider <- extensionTypingProvider

    checker.ParseAndCheckProject(ProjectOptions.typeProviderConsoleProjectOptions)
        |> Async.RunSynchronously
        |> ignore
    gotRequest |> should be True
    
    Shim.ExtensionTypingProvider <- defaultExtensionTypingShim