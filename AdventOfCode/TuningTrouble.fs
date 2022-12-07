module TuningTrouble

open Utils

type SeqState = struct
    val Head: int
    val Tail: int

    new(head, tail) = { Head = head; Tail = tail }
end


let checkUniqueSequence(searchString: string, size: int): bool =
    searchString |> Set.ofSeq |> Set.count = size

let searchForUniqueSeq(stream: string, size: int): int =
    let endIndex = stream.Length - size
    let offset = size - 1
    let range = seq { 0..endIndex } |> Seq.map(fun i -> SeqState(i, i + offset))
    let startIdx = range |> Seq.findIndex(fun state -> checkUniqueSequence(stream[state.Head..state.Tail], size))
    startIdx + size

let getDay6Solution = 
    printf "\nDAY 6\n"

    let stream = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\stream.txt" |> Seq.head
    let matchLocations = searchForUniqueSeq(stream, 4)
    let matchMessages = searchForUniqueSeq(stream, 14)

    printf "%d\n" matchLocations
    printf "%d\n" matchMessages
