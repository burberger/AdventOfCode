module CampCleanup

open Utils

let getOverlap(assignmentStr: string): Set<int> =
    let assignmentRanges = assignmentStr.Split('-', ',') |> Seq.map int |> Seq.splitInto 2
    let assignments = assignmentRanges |> Seq.map(fun assignment -> Set.ofSeq(seq {
        Seq.head assignment .. Seq.last assignment
    }))
    assignments |> Set.intersectMany

let checkFullOverlap(assignmentStr: string): int =
    let assignmentRanges = assignmentStr.Split('-', ',') |> Seq.map int |> Seq.splitInto 2
    let assignments = assignmentRanges |> Seq.map(fun assignment -> Set.ofSeq(seq {
        Seq.head assignment .. Seq.last assignment
    }))
    let overlap = assignments |> Set.intersectMany

    let result = overlap.Count = (Seq.head assignments).Count || overlap.Count = (Seq.last assignments).Count
    if result then 1 else 0

let getDay4Solution =
    printf "\nDAY 4\n"

    let cleanupAssignments = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\cleanupAssignments.txt" |> Seq.toList
    let totalOverlaps = cleanupAssignments |> Seq.map checkFullOverlap |> Seq.sum
    let totalPartialOverlaps = 
        cleanupAssignments 
        |> Seq.map getOverlap 
        |> Seq.map (fun set -> if set.Count <> 0 then 1 else 0)
        |> Seq.sum

    printf "%d\n" totalOverlaps
    printf "%d\n" totalPartialOverlaps
