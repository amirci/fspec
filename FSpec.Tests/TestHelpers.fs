namespace FSpec.Tests

open FSpec
open SpecRunner
open FsUnit
open NUnit.Framework

[<AutoOpen>]
module Helpers =
    let mutable steps:int list = []

    let step i _ = steps <- i :: steps

    let arrangeSteps _ = steps <- []

    let theSteps () = steps |> List.rev

