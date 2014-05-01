﻿module FSpec.Core.DomainTypes

module MetaData =
    type T = { Data: Map<string,obj> }
    let create data = 
        let downCastData = data |> List.map (fun (x,y) -> (x, y :> obj))
        { Data = Map<string,obj> downCastData }
    let Zero = create []
    let get<'T> name metaData = metaData.Data.Item name :?> 'T
    type T with
        member self.get<'T> name = get<'T> name self

module TestContext =
    type T = { MetaData: MetaData.T }
        with
            member self.metadata<'T> name = self.MetaData.get<'T> name

    let create metaData = { MetaData = metaData }

module Example =
    type T = {
        Name: string; 
        Test: TestContext.T -> unit;
        MetaData: MetaData.T
    }
    let create name test = { Name = name; Test = test; MetaData = MetaData.Zero }
    let addMetaData metaData example = { example with MetaData = metaData }

module ExampleGroup =
    type TestFunc = unit -> unit
    type T = {
        Name: string
        Examples: Example.T list;
        Setups: TestFunc list;
        TearDowns: TestFunc list;
        ChildGroups : T list;
        }

    let create name = { 
        Name = name;
        Examples = [];
        Setups = [];
        TearDowns = [];
        ChildGroups = [];
    }
    let addExample test ctx = { ctx with Examples = test::ctx.Examples }
    let addSetup setup ctx = { ctx with Setups = setup::ctx.Setups }
    let addTearDown tearDown ctx = { ctx with TearDowns = tearDown::ctx.TearDowns }
    let addChildContext child ctx = { ctx with ChildGroups = child::ctx.ChildGroups }

    let rec performSetup exampleGroups =
        match exampleGroups with
            | [] -> ()
            | head::tail ->
                performSetup tail
                head.Setups |> List.iter (fun y -> y())
    
    let rec performTearDown exampleGroups =
        match exampleGroups with
            | [] -> ()
            | head::tail ->
                head.TearDowns |> List.iter (fun y -> y())
                performTearDown tail
    
    let rec run exampleGroups (results : TestReport) =
        let exampleGroup = exampleGroups |> List.head
        let rec printNameStack(stack) : string =
            match stack with
            | []    -> ""
            | head::[] -> head
            | head::tail ->sprintf "%s %s" (printNameStack(tail)) head

        exampleGroup.Examples |> List.rev |> List.iter (fun example -> 
            let nameStack = example.Name :: (exampleGroups |> List.map (fun x -> x.Name) |> List.filter (fun x -> x <> null))
            let name = printNameStack(nameStack)
            try
                try
                    let context = example.MetaData |> TestContext.create
                    performSetup exampleGroups
                    example.Test context
                finally
                    performTearDown exampleGroups
                results.reportTestName name Success
            with
            | PendingError -> results.reportTestName name Pending
            | AssertionError(e) ->
                results.reportTestName name (Failure(e))
            | ex -> 
                results.reportTestName name (Error(ex))
        )

        exampleGroup.ChildGroups |> List.rev |> List.iter (fun x ->
            run (x::exampleGroups) results
        )