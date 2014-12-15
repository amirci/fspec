namespace FSpec.Examples

module SomeTests =
    open NUnit.Framework
    open System.Reflection        

    open FSpec
    open Runner
    open DSL

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


    [<Test>]
    let ``running it`` () =
        runSpecsFrom (System.Reflection.Assembly.GetExecutingAssembly())
