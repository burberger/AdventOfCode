module HillClimbing

open Utils
open System.Collections.Generic

type Point = {
    X: int
    Y: int
}

type Grid = {
    Heights: int[,]
    Start: Point
}

let Zero(): Point = 
    {X = 0; Y = 0}

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

let parseGrid(gridStr: string list): Grid =
    let ySize = gridStr.Length
    let xSize = gridStr[0].Length
    let grid = Array2D.zeroCreate ySize xSize

    let mutable startPos = {X = 0; Y = 0}
    for i in 0..ySize-1 do
        let startIdx, gridLine = parseLine(gridStr[i])
        grid[i, *] <- gridLine

        match startIdx with
        | Some idx -> startPos <- {X = idx; Y = i}
        | None -> ()

    {Heights = grid; Start = startPos}

let getNeighbors(pos, grid): Point list =
    let width = (grid.Heights.GetLength 0) - 1
    let height = (grid.Heights.GetLength 1) - 1
    seq {
        if pos.X > 0 then
            yield {X = pos.X - 1; Y = pos.Y}
        if pos.Y > 0 then
            yield {X = pos.X; Y = pos.Y - 1}
        if pos.X < width then
            yield {X = pos.X + 1; Y = pos.Y}
        if pos.Y < height then
            yield {X = pos.X; Y = pos.Y + 1}
    } |> Seq.toList

let findPath(grid: Grid): int =
    let queue = Queue()

    let rec findPath(explored: Set<Point>, grid: Grid): int = 
        if queue.Count = 0 then
            -1
        else
            let dist, point = queue.Dequeue()
            let height = grid.Heights[point.X, point.Y]

            if height = 26 then
                dist
            else
                let neighbors = getNeighbors(point, grid)
                let explored = (explored, neighbors) ||> List.fold(
                    fun explored neighbor -> 
                        let neighborHeight = grid.Heights[neighbor.X, neighbor.Y]
                        if not <| explored.Contains(neighbor) && neighborHeight <= height then
                            queue.Enqueue((dist + 1, neighbor))
                            explored.Add(neighbor)
                        else
                            explored
                )

                findPath(explored, grid)
            
    queue.Enqueue((0, grid.Start))
    let explored = Set.empty |> Set.add(grid.Start)

    findPath(explored, grid)


        

let getDay12Solution =
    printf "\nDAY 12\n"

    let gridStr = readFile @"heightMapEx.txt" |> Seq.toList
    let grid = parseGrid(gridStr)
    let result = findPath(grid)

    printfn "%A" grid
    printfn "%A" result
