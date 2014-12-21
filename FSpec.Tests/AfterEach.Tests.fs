namespace FSpec.Tests

open FSpec
open SpecRunner
open FsUnit
open NUnit.Framework

module ``After each tests`` =

    let mutable steps:int list = []

    let step i _ = steps <- i :: steps

    let arrangeSteps _ = steps <- []

    let theSteps () = steps |> List.rev

    [<Test>]
    let ``Runs the before fn after each assertion`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            after (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            it "Asserts something even further" (step 4)
        ))

        theSteps() |> should equal [2;1;3;1;4;1]


    [<Test>]
    let ``Runs the after fn before each nested context`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            after (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            context "First nested context" (fun _ ->
    
                after (step 4)
                        
                it "Asserts something even further" (step 5)
            )
        ))

        theSteps() |> should equal [2;1;3;1;5;4;1]

    [<Test>]
    let ``Runs the after fn before multiple nested contexts`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            after (step 1)

            context "Second nested context" (fun _ ->
            
                after (step 6)

                context "Third nested context" (fun _ ->
                
                    after (step 7)
                    it "Asserts the nested nested context" (step 8)
                )
            )
        ))

        theSteps() |> should equal [8;7;6;1]