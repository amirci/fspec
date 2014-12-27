namespace FSpec.Tests

open FSpec
open SpecRunner
open FsUnit
open NUnit.Framework


module ``'After all' tests`` =

    [<Test>]
    let ``Runs the 'after all' after all the assertions`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            afterAll (step 100)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            it "Asserts something even further" (step 4)
        ))

        theSteps() |> should equal [2;3;4;100]


    [<Test>]
    let ``Runs the 'after all fn' after all assertions context`` () =
        arrangeSteps()

        runSpec (describeWith (fun _ ->
            afterAll (step 100)

            it "Asserts something" (step 2)

            it "Asserts something else" (step 3)

            context "First nested context" (fun _ ->
    
                afterAll (step 200)
                        
                it "Asserts something even further" (step 5)

                it "Asserts something even further II" (step 6)
            )
        ))

        theSteps() |> should equal [2;3;5;6;200;100]

