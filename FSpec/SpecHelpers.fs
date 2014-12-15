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

    let private contextExp desc (f: BodyFn) =
        printfn "%s Context ``%s``" (stars '*') desc
        let current = {DefaultContext with 
                        Description=desc
                        Body=f
                        Parent = if Context |> Stack.empty then None else Some (currentContext())
                       }
        Context 
        |> Stack.push current
        |> ignore
        f()
        Context |> Stack.pop |> ignore
        current

    let context desc f = 
        [contextExp desc f]
        |> List.append (currentContext().Nested)
        |> ignore

    let describe desc (f: BodyFn) = contextExp desc f
            
    let describeWith (f: BodyFn) = contextExp "" f

    let before (f: BodyFn) = 
        printfn "%s Before on current context %s" (stars('+')) (currentContext().Description)
        {currentContext() with Before = f} 
        |> updateContext

    let after (f: BodyFn) = 
        printfn "%s After on current context %s" (stars('-')) (currentContext().Description)
        {currentContext() with After = f} 
        |> updateContext

    let it desc (f: BodyFn) =        
        printfn "%s Assertion on current context %s" (stars('%')) (currentContext().Description)
        {currentContext() with
            Assertions = [{Body=f;Message=desc}] 
                         |> List.append (currentContext().Assertions) 
            }
        |> updateContext


    // Spec Builder

    /// Specification
    type Spec = unit -> unit    
     
    type SpecBuilder() =
                
        member b.Delay(f : unit -> Spec) = (fun() -> f()())
        member b.Zero() = (fun() -> ())
        member b.Using(g : System.IDisposable, e) = () // Ignore for now... TODO Really need this!
    
    let spec = new SpecBuilder()