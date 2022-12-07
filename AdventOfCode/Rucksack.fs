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

let getSharedElement(itemsList: seq<string>, index: int): Option<char * int> =
    if Seq.length itemsList = index then 
        None
    else
        let group = itemsList |> Seq.skip index |> Seq.take 3
        let sharedChar = group |> Seq.map(fun x -> x |> Set.ofSeq) |> Set.intersectMany |> Set.minElement
        Some((sharedChar, index + 3))


let getDay3Solution =
    printf "\nDAY 3\n"

    let itemsList = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\rucksackItems.txt" |> Seq.toList
    let totalRepeatScore = itemsList |> Seq.map (fun x -> (getItemScore(getRepeatItem x).Value)) |> Seq.sum
    let groupChars = 0 |> Seq.unfold(fun index -> getSharedElement(itemsList, index))
    let totalGroupScore = groupChars |> Seq.map (fun x -> (getItemScore x).Value) |> Seq.sum

    printf "%d\n" totalRepeatScore
    printf "%d\n" totalGroupScore
