namespace FSpec.Examples

module ``How before and after work`` =
    open NUnit.Framework
    open System.Reflection        
    open FsUnit

    open FSpec
    open Runner
    open DSL

    let ``This is a test of describe`` = describeWith (fun _ ->

        before (fun _ ->
            printfn "Before each test (on main context)"
        )

        after (fun _ -> 
            printfn "After each test (on main context)"
        )

        context "When we are on the first scenario" (fun _ -> 
        
            it "First assertion on scenario #1" (fun _ ->
                true |> should be True
            )

            it "Second assertion on scenario #2" (fun _ ->
                1 + 2 |> should equal 3
            )
        )

        context "When we are on the second scenario" (fun _ -> 
        
            it "First Assertion on Scenario #2" (fun _ ->
                "Fspec" |> should contain "spec"
            )

            context "and the first branch happens" (fun _ ->
            
                before (fun _ ->
                    printfn "Before the branch #1 of scenario #2"
                )

                after (fun _ ->
                    printfn "After the branch #1 of scenario #2"
                )

                it "First assertion on branch #1 scneario #2" (fun _ ->
                    1 + 2 |> should equal 4
                )
            )
        )

    )


    [<Test>]
    let ``running it`` () =
        runSpecsFrom (System.Reflection.Assembly.GetExecutingAssembly())
