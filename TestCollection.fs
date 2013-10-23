module FSpec

type TestResultType =
  | Success
  | Failure

type TestReport() =
  let mutable noOfTestsRun = 0
  let mutable noOfFails = 0
  let mutable output = []

  member self.reportTestRun () =
    noOfTestsRun <- noOfTestsRun + 1

  member self.reportFailure () =
    noOfFails <- noOfFails + 1

  member self.summary() = 
    sprintf "%d run, %d failed" noOfTestsRun noOfFails

  member self.success() =
    noOfFails = 0

  member self.reportTestName name result =
    let name2 = match result with
                | Success -> sprintf "%s - passed" name
                | Failure -> sprintf "%s - failed" name
    output <- name2::output

  member self.testOutput() =
    output |> List.rev

type Test = {Name: string; test: unit -> unit}

type TestCollection(parent, name) =
  let mutable tests = []
  let mutable setups = []
  let mutable contexts = []
  let mutable current = None
  new () = TestCollection(None, null)

  member self.init (f: unit -> 'a) : (unit -> 'a) =
    let value = ref None
    self.before <| fun() ->
      value := None
    let r () =
      match !value with
      | None -> let result = f()
                value := Some(result)
                result
      | Some(x) -> x
    r

  member self.describe (name: string) (f: unit -> unit) = 
    match current with 
    | None -> let innerCollection = TestCollection(Some(self), name)
              current <- Some(innerCollection)
              f()
              current <- None
              contexts <- innerCollection::contexts
    | Some(v) -> v.describe name f

  member self.before (f: unit -> unit) =
    match current with
    | None    -> setups <- f::setups
    | Some(v) -> v.before f

  member self.it (name: string) (f: unit -> unit) = 
    match current with
    | None    -> tests <- {Name = name; test = f}::tests
    | Some(v) -> v.it name f

  member self.perform_setup() =
    match parent with
    | None    -> ()
    | Some(x) -> x.perform_setup()
    setups |> List.iter (fun y -> y())

  member self.nameStack () =
    match parent with
    | None    -> []
    | Some(x) -> name::x.nameStack()

  member self.run(results : TestReport) =
    let rec printNameStack(stack) : string =
        match stack with
        | []    -> ""
        | head::[] -> head
        | head::tail ->sprintf "%s %s" (printNameStack(tail)) head

    tests |> List.rev |> List.iter (fun x -> 
      self.perform_setup()
      results.reportTestRun()
      let nameStack = x.Name :: self.nameStack()
      let name = printNameStack(nameStack)
      try
          x.test()
          results.reportTestName name Success
      with
      | ex -> 
          results.reportFailure()
          results.reportTestName name Failure
    )

    contexts |> List.rev |> List.iter (fun x ->
      x.run(results)
    )

  member self.run() = 
    self.run(TestReport())
