namespace AdventOfCode2021.Days

open AdventOfCode2021.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module Day02 =

    type Direction =
        | Forward of int
        | Down of int
        | Up of int
        static member ofString (str : string) =
            let split = str.Split " "
            let name = split[0]
            let value = split[1] |> int
            match name.ToLower() with
            | "forward" -> Forward value
            | "down" -> Down value
            | "up" -> Up value
            | unknown -> failwith $"Unknown direction ${unknown}"

    type Position =
        {
            Horizontal: int
            Depth: int
            Aim: int
        }
        static member createDefault() =
            {
                Horizontal = 0
                Depth = 0
                Aim = 0
            }
        static member multiply pos =
            pos.Horizontal * pos.Depth

    let commonPart (input: string[]) folder =
        input
        |> Array.map Direction.ofString
        |> Array.fold folder (Position.createDefault())
        |> Position.multiply

    let part1 (input: string[]) =
        commonPart input (fun pos cmd ->
            match cmd with
            | Forward x -> { pos with Horizontal = pos.Horizontal + x }
            | Up x -> { pos with Depth = pos.Depth - x }
            | Down x -> { pos with Depth = pos.Depth + x }
            )

    let part2 (input: string[]) =
        commonPart input (fun pos cmd ->
            match cmd with
            | Forward x -> { pos with Horizontal = pos.Horizontal + x
                                      Depth = pos.Depth + (pos.Aim * x) }
            | Up x -> { pos with Aim = pos.Aim - x }
            | Down x -> { pos with Aim = pos.Aim + x }
            )


    type Tests(output:ITestOutputHelper) =
        [<Fact>]
        let testPart1() =
            Inputs02.test |> parseInput |> part1 |> should equal 150

        [<Fact>]
        let taskPart1() =
            Inputs02.task |> parseInput |> part1 |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            Inputs02.test |> parseInput |> part2 |> should equal 900

        [<Fact>]
        let taskPart2() =
            Inputs02.task |> parseInput |> part2 |> string |> output.WriteLine
