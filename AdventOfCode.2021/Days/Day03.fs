namespace AdventOfCode2021.Days

open System.Collections
open AdventOfCode2021.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module Day03 =
    let getInt (bitArray: BitArray) =
        let array : int[] = Array.create 1 0
        bitArray.CopyTo(array, 0)
        array[0]

    let part1 (bitRows : bool[][]) =
        let gamma =
            bitRows
            |> Array.transpose
            |> Array.map (Array.countBy id >> Map.ofSeq)
            |> Array.map (fun m -> (Map.find true m) > (Map.find false m))
            |> BitArray

        let gammaRate = gamma |> getInt
        let epsilonRate = gamma.Not() |> getInt
        gammaRate * epsilonRate

    type Tests(output:ITestOutputHelper) =

        let mapper (row : string) = row.ToCharArray() |> Array.map (fun c -> c = '1') |> Array.rev
        [<Fact>]
        let testPart1() =
            Inputs03.test |> parseInputMap mapper |> part1 |> should equal 198

        [<Fact>]
        let taskPart1() =
            Inputs03.task |> parseInputMap mapper |> part1 |> string |> output.WriteLine
