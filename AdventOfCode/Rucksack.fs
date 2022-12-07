module Rucksack

open Utils

let getItemScore(item: char): Option<int> =
    match item with
    | c when c >= 'a' && c <= 'z' -> Some(int c - 96)
    | c when c >= 'A' && c <= 'Z' -> Some(int c - 38)
    | _ -> None

let getRepeatItem(itemStr: string): char =
    let splitBag = itemStr |> Seq.splitInto 2

    let firstHalfItems = splitBag |> Seq.head |> Set.ofSeq
    let lastHalfItems = splitBag |> Seq.last |> Set.ofSeq

    (Set.intersect firstHalfItems lastHalfItems).MinimumElement

let getSharedElement(itemsList: seq<string>) =
    sharedElement


let getDay3Solution =
    printf "\nDAY 3\n"

    let itemsList = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\rucksackItems.txt"
    let totalRepeatScore = itemsList |> Seq.map (fun x -> (getItemScore(getRepeatItem x).Value)) |> Seq.sum
    printf "%d\n" totalRepeatScore
