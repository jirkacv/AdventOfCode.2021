namespace AdventOfCode2021.Days

open System
open Xunit
open FsUnit.Xunit
open AdventOfCode2021.Inputs
open Xunit.Abstractions

module Day01 =
    let part1 input =
        input
        |> Array.fold (fun (lastVal, result) cur ->
            let res = if cur > lastVal then result + 1 else result
            (cur, res)) (Int32.MaxValue, 0)
        |> snd

    let part2 (input : int[]) =
        input
        |> Array.windowed 3
        |> Array.map Array.sum
        |> part1


    type Tests(output:ITestOutputHelper) =
        [<Fact>]
        let testPart1() =
            Inputs01.test |> parseInputMap int |> part1 |> should equal 7

        [<Fact>]
        let taskPart1() =
            Inputs01.task |> parseInputMap int |> part1 |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            Inputs01.test |> parseInputMap int |> part2 |> should equal 5


        [<Fact>]
        let taskPart2() =
            Inputs01.task |> parseInputMap int |> part2 |> string |> output.WriteLine
