module SupplyStacks

open Utils

type StackSet = List<List<char>>
type CrateLine = List<Option<char>>
type MoveInstruction = int * int * int

let moveCrates(crates: StackSet, instruction: MoveInstruction, reverse: bool): StackSet =
    let count, startIdx, endIdx = instruction
    let movedCrates = crates[startIdx] |> List.take count
    let movedCrates = if reverse then movedCrates |> List.rev else movedCrates
    Seq.toList(seq {
        for x in 0..(crates.Length - 1) do
            if x = startIdx then
                yield crates[x] |> List.skip count
            elif x = endIdx then
                yield movedCrates @ crates[x]
            else
                yield crates[x]
    })

let getNumStacks(crateLine: string): int =
    (crateLine.Length + 1) / 4

let parseCrateLine(crateLine: string): CrateLine =
    let numStacks = getNumStacks crateLine
    let crates = crateLine |> Seq.splitInto numStacks
    
    Seq.toList(seq {
        for crate in crates do
            if crate |> Seq.exists(fun x -> x = '[') then
                yield Some(crate.[1])
            else
                yield None

    })

let appendCrateLayer(crates: StackSet, crateLine: CrateLine): StackSet =
    (crates, crateLine) ||> Seq.map2(fun crateStack crate -> 
        if crate.IsSome then 
            crateStack @ [crate.Value]
        else
            crateStack
    ) |> Seq.toList

let buildStackSet(fileContents: seq<string>): StackSet =
    let stackLines = fileContents |> Seq.filter (fun line -> line |> Seq.contains '[')
    ([for _ in 1..getNumStacks(stackLines |> Seq.head) -> []], stackLines) ||> 
        Seq.fold(fun stack line -> appendCrateLayer(stack, parseCrateLine line))

let parseMoveCommand(moveLine: string): MoveInstruction =
    let numbers = moveLine.Split ' '
    (int numbers[1], (int numbers[3]) - 1, (int numbers[5]) - 1)

let buildMoveSet(fileContents: seq<string>): seq<MoveInstruction> = 
    let moveLines = fileContents |> Seq.filter (fun line -> line |> Seq.contains 'm')
    moveLines |> Seq.map(fun line -> parseMoveCommand line)

let getTopCrates(crates: StackSet): string =
    ("", crates) ||> List.fold(fun str stack -> str + (stack |> List.head |> string))


let getDay5Solution =
    printf "\nDAY 5\n"

    let contents = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\stackInstructions.txt" |> Seq.toList
    let stackSet = buildStackSet contents
    let moves = buildMoveSet contents
    let revFinalCrates = (stackSet, moves) ||> Seq.fold(fun stackSet move -> moveCrates(stackSet, move, true))
    let fwdFinalCrates = (stackSet, moves) ||> Seq.fold(fun stackSet move -> moveCrates(stackSet, move, false))
    let revTopCrates = getTopCrates revFinalCrates
    let fwdTopCrates = getTopCrates fwdFinalCrates

    printf "%A\n" revTopCrates
    printf "%A\n" fwdTopCrates

