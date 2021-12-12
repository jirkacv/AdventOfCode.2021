namespace AdventOfCode2021.Days

open System.Collections
open AdventOfCode2021.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module Day03 =
    let toBitArray = Array.rev<bool> >> BitArray

    let toInt (bitArray: BitArray) =
        let array = Array.create 1 0
        bitArray.CopyTo(array, 0)
        array[0]

    let findValueWithDefault value map = Map.tryFind value map |> Option.defaultValue 0

    let getMostCommonBits (bitRows : bool[][]) =
        bitRows
        |> Array.transpose
        |> Array.map (Array.countBy id >> Map.ofSeq)
        |> Array.map (fun m -> (findValueWithDefault true m) >= (findValueWithDefault false m))


    let part1 (bitRows : bool[][]) =
        let gamma =
            bitRows
            |> getMostCommonBits
            |> toBitArray
        let gammaRate = gamma |> toInt
        let epsilonRate = gamma.Not() |> toInt
        gammaRate * epsilonRate

    let part2 (bitRows : bool[][]) =
        let rec filterRows mostCommon (currentBitRows : bool[][]) position =
            let mostCommonBits = currentBitRows |> getMostCommonBits
            let filtered =
                currentBitRows
                |> Array.filter (fun i ->
                    if mostCommon
                    then i[position] = mostCommonBits[position]
                    else i[position] <> mostCommonBits[position])
            match filtered with
            | [| last |] -> last
            | more -> filterRows mostCommon more (position + 1)

        let oxygen =
            filterRows true bitRows 0
            |> toBitArray
            |> toInt
        let co2 =
            filterRows false bitRows 0
            |> toBitArray
            |> toInt
        oxygen * co2



    type Tests(output:ITestOutputHelper) =

        let mapper (row : string) = row.ToCharArray() |> Array.map (fun c -> c = '1')
        [<Fact>]
        let testPart1() =
            Inputs03.test |> parseInputMap mapper |> part1 |> should equal 198

        [<Fact>]
        let taskPart1() =
            Inputs03.task |> parseInputMap mapper |> part1 |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            Inputs03.test |> parseInputMap mapper |> part2 |> should equal 230

        [<Fact>]
        let taskPart2() =
            Inputs03.task |> parseInputMap mapper |> part2 |> string |> output.WriteLine
