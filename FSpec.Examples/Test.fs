namespace FSpec.Examples

open FSpec


module SomeTests =

    let ``The test does something`` = describeWith (fun _ ->

        before (fun _ ->
            printfn "This runs before"
        )

        after (fun _ -> 
            printfn "This runs after"
        )

        context "when the path is something" (fun _ -> 
        
            it "does what we expected" (fun _ ->
                printfn "Indeed it does"
            )

            it "does something else" (fun _ ->
                printfn "Indeed is something else"
            )
        )

        context "when the path is other" (fun _ -> 
        
            it "does what it didn't do" (fun _ ->
                printfn "It didn't do it or I would know"
            )

            context "and the branch happens" (fun _ ->
            
                before (fun _ ->
                    printfn "before the branch"
                )

                after (fun _ ->
                    printfn "after the branch"
                )
                it "went the to branch" (fun _ ->
                    printfn "and did the branch too"
                )
            )
        )

    )

module Runners =
    open NUnit.Framework
    open System.Reflection        
    open Microsoft.FSharp.Reflection
    open System.Text.RegularExpressions
    open FsUnit
                                      
    [<Test>]
    let ``running it`` () =
        let onlySpecs (mi:MemberInfo) =
            match mi with
            | :? PropertyInfo as pi -> pi.PropertyType = typeof<TestSuite>
            | _ -> false


        let rec runSpec (suite:TestSuite) =

            suite.Assertions
            |> Seq.iter (fun itShould -> 
                suite.Before()
                printfn "%s" itShould.Message
                itShould.Body()
                suite.After()
            )

            suite.Children 
            |> Seq.iter runSpec


        let asm = System.Reflection.Assembly.GetExecutingAssembly()                    
        asm.GetTypes()
        |> Seq.filter(fun t -> (FSharpType.IsModule t))
        |> Seq.map(fun t -> t.GetMembers())
        |> Seq.concat
        |> Seq.filter onlySpecs
        |> Seq.map (fun mi -> (mi :?> PropertyInfo).GetValue(null) :?> TestSuite)
        |> Seq.iter runSpec

