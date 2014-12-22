namespace FSpec

module Stack =
    let create<'T> () = new System.Collections.Generic.Stack<'T>()
    
    let push e (stack:System.Collections.Generic.Stack<'T>) = 
        stack.Push(e)
        stack

    let pop (stack:System.Collections.Generic.Stack<'T>) = stack.Pop(), stack

    let peek (stack:System.Collections.Generic.Stack<'T>) = stack.Peek()

    let empty (stack:System.Collections.Generic.Stack<'T>) = stack.Count = 0
    
[<AutoOpen>]
module DSL =
           
    type private BodyFn = unit -> unit

    let private EmptyFn = id

    type Assertion = {Body:BodyFn; Message:string}

    type Spec = {
        Description:string
        Body: BodyFn
        Before: BodyFn
        After: BodyFn
        BeforeAll: BodyFn
        AfterAll: BodyFn
        Assertions: Assertion list
        Nested: Spec list
        Parent: Spec option
    }

    type TestSuite = Spec

    let private DefaultContext = {Description = ""; 
                    Body        = EmptyFn;
                    Before      = EmptyFn;
                    After       = EmptyFn;
                    BeforeAll   = EmptyFn;
                    AfterAll    = EmptyFn;
                    Assertions  = [];
                    Nested      = [];
                    Parent      = None}

    let mutable private Context = Stack.create<Spec>()

    let private currentContext() = Context |> Stack.peek

    let private updateContext update = 
        Context
        |> Stack.pop
        |> snd
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
        
        Context 
        |> Stack.pop 
        |> fst

    let context desc f = 
        {currentContext() with 
            Nested = [contextExp desc f] |> List.append (currentContext().Nested)
        } 
        |> updateContext

    let describe desc (f: BodyFn) = contextExp desc f
            
    let describeWith (f: BodyFn) = contextExp "" f

    let beforeAll(f:BodyFn) =
        {currentContext() with BeforeAll = f} 
        |> updateContext

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

