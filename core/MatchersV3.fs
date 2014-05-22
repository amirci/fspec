module FSpec.Core.MatchersV3

[<AbstractClass>]
type Matcher<'TActual> () = 
    abstract member ApplyActual<'TResult> : (bool -> 'TResult) -> 'TActual -> 'TResult
    abstract member FailureMsgForShould : string
    abstract member FailureMsgForShouldNot : string
    default this.FailureMsgForShouldNot : string = sprintf "not %s" this.FailureMsgForShould

let applyMatcher<'T> (matcher: Matcher<'T>) f (a : 'T) =
    matcher.ApplyActual f a

let createFullMatcher<'T> 
        (f : 'T -> bool) 
        (shouldMsg : string) 
        (shouldNotMsg : string) =
    { new Matcher<'T> () with
        member __.ApplyActual g actual = f actual |> g
        member __.FailureMsgForShould = shouldMsg
        member __.FailureMsgForShouldNot = shouldNotMsg
    }
let createMatcher<'T> (f : 'T -> bool) (shouldMsg : string) =
    { new Matcher<'T> () with
        member __.ApplyActual g actual = f actual |> g
        member __.FailureMsgForShould = shouldMsg
    }

let createSimpleMatcher f = createMatcher f "FAIL"
        
module be =
    let equalTo expected =
        let f a = a = expected
        createMatcher f (sprintf "be equal to %A" expected)

    let True =
        createMatcher 
            (fun actual -> actual = true) 
            (sprintf "be true")

    let False =
        createMatcher 
            (fun actual -> actual = false)
            (sprintf "be false")

    module string =
        let containing expected =
            createMatcher
                (fun (a:string) -> a.Contains(expected))
                (sprintf "contain %s" expected)

        let matching pattern =
            let regex = System.Text.RegularExpressions.Regex pattern
            createMatcher
                (fun actual -> regex.IsMatch actual)
                (sprintf "match regex pattern %A" pattern)

module have =
    let atLeastOneElement matcher =
        let f a = a |> Seq.exists (applyMatcher matcher id)
        let msg = sprintf "contain at least one element to %s" matcher.FailureMsgForShould
        let notMsg = sprintf "contain no elements to %s" matcher.FailureMsgForShould
        createFullMatcher f msg notMsg
    
    let length matcher =
        let f a = a |> Seq.length |> applyMatcher matcher id
        let msg = sprintf "have length to %s" matcher.FailureMsgForShould
        createMatcher f msg

    let exactly no matcher =
        let f a = a |> Seq.filter (applyMatcher matcher id) |> Seq.length = no
        let msg = 
            sprintf "contain exactly %d element to %s" no 
                matcher.FailureMsgForShould
        createMatcher f msg

let fail =
    let f a =
        try
            a (); false
        with
        | _ -> true
    createMatcher f "fail"

module throwException =
    let withMessage matcher =
        let f a = 
            try
                a ()
                false
            with
            | e -> e.Message |> applyMatcher matcher id
        createMatcher f
            (sprintf "throw exception with message %s" matcher.FailureMsgForShould)
            
    let withMessageContaining msg =
        withMessage (be.string.containing msg)
    
let shouldNot<'T> (matcher:Matcher<'T>) (actual:'T) =
    let continuation = function
        | false -> ()
        | true -> 
            let msg = sprintf "Expected %A to %s" actual matcher.FailureMsgForShouldNot
            raise (AssertionError { Message = msg })
    matcher.ApplyActual continuation actual
    
let should<'T> (matcher:Matcher<'T>) (actual:'T) =
    let continuation = function
        | true -> ()
        | false -> 
            let msg = sprintf "Expected %A to %s" actual matcher.FailureMsgForShould
            raise (AssertionError { Message = msg })
    matcher.ApplyActual continuation actual

/// Extension methods for System.Object to aid in assertions
type System.Object with
    /// Allows the use of testContext.Subject.Should (matcher)
    member self.Should<'T> (matcher : Matcher<'T>) =
        self :?> 'T |> should matcher

    /// Allows the use of testContext.Subject.ShouldNot (matcher)
    member self.ShouldNot<'T> (matcher : Matcher<'T>) =
        self :?> 'T |> shouldNot matcher
