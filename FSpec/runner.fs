namespace FSpec

module Runner =

    open Microsoft.FSharp.Reflection
    open System.Reflection        

    open DSL

    let runSpecsFrom (asm:Assembly) =                                      
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

            
            suite.Nested 
            |> Seq.iter runSpec


        asm.GetTypes()
        |> Seq.filter(fun t -> (FSharpType.IsModule t))
        |> Seq.map(fun t -> t.GetMembers())
        |> Seq.concat
        |> Seq.filter onlySpecs
        |> Seq.map (fun mi -> (mi :?> PropertyInfo).GetValue(null) :?> TestSuite)
        |> Seq.iter runSpec

