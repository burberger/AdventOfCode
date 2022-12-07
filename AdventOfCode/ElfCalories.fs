module ElfCalories

open System.IO;

let getElfCalories (filename: string): seq<int> = 
    let caloriesFile = new StreamReader(filename) |> Seq.unfold (
        fun sr -> 
            match sr.ReadLine() with
            | null -> None
            | str when String.length str <> 0 -> Some(int str, sr)
            | _ -> Some(0, sr)
    )

    seq {
        let mutable sum = 0
        for x in caloriesFile do
            match x with
            | x when x = 0 -> yield sum; sum <- 0
            | x -> sum <- sum + x
    }

let getSortedElfCalories (filename: string): int list =
    getElfCalories filename |> Seq.toList |> List.sortDescending


let getDay1Solution = 
    let calories = getSortedElfCalories @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\elfCalories.txt"

    let max = calories[0]
    let topThreeSum = calories |> List.take 3 |> List.sum

    printf "\nDAY 1\n"
    printf "%d\n" max
    printf "%d\n" topThreeSum
