namespace AdventOfCode2021.Inputs

open System

[<AutoOpen>]
module Common =
    let parseInput (input : string) = input.Split Environment.NewLine
    let parseInputMap mapper (input : string) = input |> parseInput |> Array.map mapper
