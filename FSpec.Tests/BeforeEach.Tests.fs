namespace FSpec.Tests

open FSpec
open SpecRunner
open FsUnit
open NUnit.Framework

module ``Before each tests`` =

    let mutable steps:int list = []

    let step i _ = steps <- i :: steps

    let arrangeSteps _ = steps <- []

    let theSteps () = steps |> List.rev

    [<Test>]
    let ``Runs the before fn before each assertion`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            before (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            it "Asserts something even further" (step 4)
        ))

        theSteps() |> should equal [1;2;1;3;1;4]


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

        theSteps() |> should equal [1;2;1;3;1;4;5]

    [<Test>]
    let ``Runs the before fn before multiple nested contexts`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            before (step 1)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            context "First nested context" (fun _ ->
    
                before (step 4)
                        
                it "Asserts something even further" (step 5)
            )

            context "Second nested context" (fun _ ->
            
                before (step 6)

                context "Third nested context" (fun _ ->
                
                    before (step 7)
                    it "Asserts the nested nested context" (step 8)
                )
            )
        ))

        theSteps() |> should equal [1;2;1;3;1;4;5;1;6;7;8]