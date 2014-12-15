namespace FSpec

module Stack =
    let create<'T> () = new System.Collections.Generic.Stack<'T>()
    
    let push e (stack:System.Collections.Generic.Stack<'T>) = 
        stack.Push(e)
        stack

    let pop (stack:System.Collections.Generic.Stack<'T>) = 
        stack.Pop() |> ignore
        stack

    let peek (stack:System.Collections.Generic.Stack<'T>) = stack.Peek()

    let empty (stack:System.Collections.Generic.Stack<'T>) = stack.Count = 0
    
[<AutoOpen>]
module DSL =
           
    type private BodyFn = unit -> unit

    let private EmptyFn = id

    type Assertion = {Body:BodyFn; Message:string}

    type ContextSpec = {
        Description:string
        Body: BodyFn
        Before: BodyFn
        After: BodyFn
        Assertions: Assertion list
        mutable Nested: ContextSpec list
        Parent: ContextSpec option
    }

    type TestSuite = ContextSpec

    let private DefaultContext = {Description = ""; 
                    Body        = EmptyFn;
                    Before      = EmptyFn;
                    After       = EmptyFn;
                    Assertions  = [];
                    Nested      = [];
                    Parent      = None}

    let mutable private Context = Stack.create<ContextSpec>()

    let private currentContext() = Context |> Stack.peek

    let private updateContext update = 
        Context
        |> Stack.pop
        |> Stack.push update
        |> ignore

    let stars c =
        let length = (Context |> Seq.length) + 1
        new System.String(c, 3 * length)

    let printSpec suite =            
        printfn "\n)))) spec --%s--" suite.Description

        printfn "  >>> Assertions %d" (suite.Assertions |> Seq.length)
        printfn "  >>> Nested %d" (suite.Nested |> Seq.length)
        printfn "  >>> Before %O" suite.Before
        printfn "  >>> After  %O" suite.After

    let private contextExp desc (f: BodyFn) =
        let newContext = {DefaultContext with 
                            Description=desc
                            Body=f
                            Parent = if Context |> Stack.empty then None else Some (currentContext())
                           }
        Context 
        |> Stack.push newContext
        |> ignore
        f()
        let current = Context |> Stack.peek
        Context |> Stack.pop |> ignore
        current


    let context desc f = 
        currentContext().Nested <- [contextExp desc f]
                                   |> List.append (currentContext().Nested)

    let describe desc (f: BodyFn) = contextExp desc f
            
    let describeWith (f: BodyFn) = contextExp "" f

    let before (f: BodyFn) = 
        {currentContext() with Before = f} 
        |> updateContext

    let after (f: BodyFn) = 
        {currentContext() with After = f} 
        |> updateContext

    let it desc (f: BodyFn) =        
        {currentContext() with
            Assertions = [{Body=f;Message=desc}] 
                         |> List.append (currentContext().Assertions) 
            }
        |> updateContext

