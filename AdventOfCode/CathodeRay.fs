module CathodeRay

open Utils

type CPUState = {
    Cycle: int;
    X: int;
}

type Instruction =
    | Addx of arg: int
    | Noop

let noop(state: CPUState): CPUState = 
    { Cycle = state.Cycle + 1; X = state.X }

let addx(state: CPUState, arg: int): CPUState =
    { Cycle = state.Cycle + 2; X = state.X + arg }

let rec processInstructions(instructions: List<Instruction>): List<CPUState> =
    let rec processInstructions(state: CPUState, instructions: List<Instruction>): List<CPUState> =
        if List.isEmpty instructions then
            []
        else
            let instruction = instructions |> List.head
            match instruction with
            | Addx x -> 
                let firstCycle = { Cycle = state.Cycle + 1; X = state.X }
                let secondCycle = { Cycle = state.Cycle + 2; X = state.X }
                let nextState = { Cycle = state.Cycle + 2; X = state.X + x }
                firstCycle :: secondCycle :: processInstructions(nextState, instructions |> List.tail)
            | Noop ->
                let newState = { Cycle = state.Cycle + 1; X = state.X }
                newState :: processInstructions(newState, instructions |> List.tail)

    processInstructions({ Cycle = 0; X = 1 }, instructions)

let parseInstructions(insText: List<string>): List<Instruction> = 
    insText |> List.map(fun ins -> 
        match ins with
        | "noop" -> Instruction.Noop
        | str -> Instruction.Addx ((str.Split ' ') |> Array.last |> int)
    )

let scoreState(state: CPUState): int =
    state.Cycle * state.X

let scoreTargetPositions(states: List<CPUState>): int =
    let scores = (seq {
        for i in 20..40..220 do
            yield states[i - 1]
    } |> Seq.map(scoreState)) |> Seq.toList

    scores |> Seq.sum

let spriteInRange(state: CPUState): bool =
    let pixelPos = (state.Cycle - 1) % 40
    abs (state.X - pixelPos) <= 1

let pixelStates(states: List<CPUState>): string =
    states |> List.map(spriteInRange) |> List.map(fun pixelState -> if pixelState then "#" else ".") |> String.concat ""

let displayPixels(pixels: string) =
    pixels |> String.iteri(
        fun i c -> if i % 40 = 0 && i <> 0 then printf "\n%c" c else printf "%c" c
    )

let getDay10Solution =
    printf "\nDAY 10\n"

    let insText = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\crtInstructions.txt" |> Seq.toList
    let instructions = parseInstructions insText
    let states = processInstructions instructions
    let score = scoreTargetPositions states
    let pixels = pixelStates states

    //states |> List.iter(fun state -> printf "%A\n" state)
    printf "%d\n" score
    displayPixels pixels

