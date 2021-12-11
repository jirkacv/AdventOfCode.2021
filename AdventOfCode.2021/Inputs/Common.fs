﻿namespace AdventOfCode2021.Inputs

[<AutoOpen>]
module Common =
    let parseInput (input : string) = input.Split "\n"
    let parseInputMap mapper (input : string) = input |> parseInput |> Array.map mapper
