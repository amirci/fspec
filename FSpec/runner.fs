namespace FSpec

module Runner =

    open Microsoft.FSharp.Reflection
    open System.Reflection        

    open DSL

    let runSpec = 
        let runAssertion spec tabs assertion = 
            printfn "%s %s" tabs assertion.Message

            spec.Before()

            let status = try 
                            assertion.Body()
                            "Success!"
                         with
                            | ex -> "Failed!"

            printfn "%s %s" tabs status

            spec.After()

        let rec runSpecWithLevel nestLevel (suite:TestSuite) =

            let tabs = new System.String(' ', 3 * nestLevel)

            printfn "%s %s" tabs suite.Description

            let tabs = new System.String(' ', 3 * (nestLevel + 1))

            suite.Assertions
            |> Seq.iter (runAssertion suite tabs) 
            
            suite.Nested 
            |> Seq.iter (runSpecWithLevel (nestLevel + 1))


        runSpecWithLevel 0

    let runSpecsFrom (asm:Assembly) =                                      
        let onlySpecs (mi:MemberInfo) =
            match mi with
            | :? PropertyInfo as pi -> pi.PropertyType = typeof<TestSuite>
            | _ -> false


        asm.GetTypes()
        |> Seq.filter(fun t -> (FSharpType.IsModule t))
        |> Seq.map(fun t -> t.GetMembers())
        |> Seq.concat
        |> Seq.filter onlySpecs
        |> Seq.map (fun mi -> 
            let pi = mi :?> PropertyInfo
            let suite = pi.GetValue(null) :?> TestSuite
            {suite with Description = pi.Name}
            )
        |> Seq.iter runSpec

