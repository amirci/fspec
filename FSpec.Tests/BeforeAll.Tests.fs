namespace FSpec.Tests

open FSpec
open SpecRunner
open FsUnit
open NUnit.Framework

module ``Before all tests`` =

    let mutable steps:int list = []

    let step i _ = steps <- i :: steps

    let arrangeSteps _ = steps <- []

    let theSteps () = steps |> List.rev

    [<Test>]
    let ``Runs the before all before all assertion`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            beforeAll (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            it "Asserts something even further" (step 4)
        ))

        theSteps() |> should equal [1;2;3;4]


    [<Test>]
    let ``Runs the before fn before all assertions context`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            beforeAll (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            context "First nested context" (fun _ ->
    
                beforeAll (step 4)
                        
                it "Asserts something even further" (step 5)

                it "Asserts something even further II" (step 6)
            )
        ))

        theSteps() |> should equal [1;2;3;4;5;6]

