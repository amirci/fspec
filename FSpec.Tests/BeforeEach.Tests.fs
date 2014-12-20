namespace FSpec.Tests

open FSpec
open Runner
open FsUnit
open NUnit.Framework

module ``Before each tests`` =

    let mutable steps:int list = []

    let step i _ = steps <- i :: steps

    let arrangeSteps _ = steps <- []

    [<Test>]
    let ``Runs the before fn before each assertion`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            before (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            it "Asserts something even further" (step 4)
        ))

        steps |> List.rev |> should equal [1;2;1;3;1;4]


    [<Test>]
    let ``Runs the before fn before each nested context`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            before (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            context "First nested context" (fun _ ->
    
                before (step 4)
                        
                it "Asserts something even further" (step 5)
            )
        ))

        steps |> List.rev |> should equal [1;2;1;3;1;4;5]