module HillClimbing

open Utils
open System.Collections.Generic

type Direction =
    | Up
    | Down
    | Left
    | Right

type Node = {
    Up: Node option;
    Down: Node option;
    Left: Node option;
    Right: Node option;
    Height: int;
}

type Grid = {
    Heights: int[,]
    Start: int * int
}

let parseLine(line: string): int option * int array =
    let mutable startIdx = None
    (startIdx, seq {
        for i in 0..line.Length-1 do
            if line[i] = 'S' then
                startIdx <- Some(i)
                yield 0
            elif line[i] = 'E' then
                yield 26
            else
                yield (int line[i] - 97)
    } |> Seq.toArray)

let parseGrid(gridStr: List<string>): Grid =
    let ySize = gridStr.Length
    let xSize = gridStr[0].Length
    let grid = Array2D.zeroCreate ySize xSize

    let mutable startPos = (0, 0)
    for i in 0..ySize-1 do
        let startIdx, gridLine = parseLine(gridStr[i])
        grid[i, *] <- gridLine

        match startIdx with
        | Some idx -> startPos <- (idx, i)
        | None -> ()

    {Heights = grid; Start = startPos}

let buildTraversal(grid: Grid): Node =

    let queue = Queue()
        

let getDay12Solution =
    printf "\nDAY 12\n"

    let gridStr = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\heightMapEx.txt" |> Seq.toList
    let grid = parseGrid(gridStr)
    let result = traverseGrid(grid)

    printfn "%A" grid
    printfn "%d" result
