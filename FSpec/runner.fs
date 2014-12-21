namespace FSpec

module SpecRunner =

    open Microsoft.FSharp.Reflection
    open System.Reflection        
    open System.IO

    open DSL

    type Events =
        | AssertionStart of string
        | AssertionPassed
        | AssertionFailed of System.Exception
        | SpecStart
        | SpecEnd
        | ContextStart
        | SuiteStarts

    type Context = {Parents:TestSuite list; Notifier: INotifier} with

        member this.Description = match this.Parents with
                                    | p::xs -> p.Description
                                    | _ -> ""

    and INotifier =
        abstract member Notify: Events -> Context -> unit

    type SilentNotifications() =
        interface INotifier with
            member this.Notify event ctxt = ()

    type NotifyToWriter (writer:TextWriter) =
        let writeIndented nestLevel msg = 
            let indent = new System.String(' ', 3 * nestLevel)
            writer.WriteLine(sprintf "%s%s" indent msg) 

        interface INotifier with
            member this.Notify event ctxt = 
                let nestLevel = (ctxt.Parents |> Seq.length)
                let write = writeIndented nestLevel

                match event with
                | AssertionStart msg -> write msg
                | AssertionPassed    -> write "Passed!"
                | AssertionFailed e  -> write "Failed!"
                | ContextStart       -> write ctxt.Description 
                | _                  -> ()

    module Notify =

        let notify event ctx = 
            ctx.Notifier.Notify event ctx
            ctx

        let assertion message = notify (AssertionStart(message))

        let passed = notify AssertionPassed

        let failed e = notify (AssertionFailed(e))

        let context = notify ContextStart

    [<AutoOpen>]
    module private TestContext =
        let before (spec:Spec) = spec.Before()
        let after  (spec:Spec) = spec.After()

        let Default = {Parents=[];Notifier=SilentNotifications()}

        let addParent spec ctx = {ctx with Parents=spec::ctx.Parents}

    module Parents =
        let iter fn ctx =
            ctx.Parents 
            |> List.iter fn

            ctx

    module Ancestors =
        let iter fn ctx =
            ctx.Parents 
            |> List.rev 
            |> List.iter fn

            ctx

    let private run (ctx:Context) (assertion:Assertion) = 
        
        let eval ctx =
            ctx
            |> try assertion.Body(); Notify.passed
               with
               | ex -> Notify.failed ex

        ctx 
        |> Notify.assertion assertion.Message
        |> Ancestors.iter before
        |> eval
        |> Parents.iter after
        |> ignore

    let rec private runSpecWithContext (ctx:Context) (spec:Spec) =

        let runAssertions ctx = spec.Assertions |> Seq.iter (run ctx) ; ctx
            
        let runNestedSpecs ctx = spec.Nested |> Seq.iter (runSpecWithContext ctx)
        
        ctx 
        |> addParent spec
        |> Notify.context
        |> runAssertions
        |> runNestedSpecs


    let runSpec = runSpecWithContext {TestContext.Default with 
                                        Notifier=NotifyToWriter System.Console.Out}

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

