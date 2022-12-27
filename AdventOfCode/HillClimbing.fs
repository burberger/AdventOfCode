module HillClimbing

open Utils
open System.Collections.Generic

type Point = {
    X: int
    Y: int
}

type Grid = {
    Heights: int[,]
    Start: Point list
    End: Point
}

let Zero(): Point = 
    {X = 0; Y = 0}

let parseLine(line: string): int option * int option * int array =
    let mutable startIdx = None
    let mutable endIdx = None
    let convertedArray = 
        seq {
            for i in 0..line.Length-1 do
                if line[i] = 'S' then
                    startIdx <- Some(i)
                    yield 0
                elif line[i] = 'E' then
                    endIdx <- Some(i)
                    yield 25
                else
                    yield (int line[i] - 97)
        } |> Seq.toArray

    (startIdx, endIdx, convertedArray)

let parseGrid(gridStr: string list): Grid =
    let ySize = gridStr.Length
    let xSize = gridStr[0].Length
    let grid = Array2D.zeroCreate xSize ySize

    let mutable startPos = Zero()
    let mutable endPos = Zero()
    for i in 0..ySize-1 do
        let startIdx, endIdx, gridLine = parseLine(gridStr[i])
        grid[*, i] <- gridLine

        match startIdx with
        | Some idx -> startPos <- {X = idx; Y = i}
        | None -> ()

        match endIdx with
        | Some idx -> endPos <- {X = idx; Y = i}
        | None -> ()

    {Heights = grid; Start = [startPos]; End = endPos}

let findAllStarts(grid: Grid): Grid =
    let xMax = (grid.Heights.GetLength 0) - 1
    let yMax = (grid.Heights.GetLength 1) - 1

    let starts = 
        seq {
            for i in 0..xMax do
                for j in 0..yMax do
                    if grid.Heights[i, j] = 0 then
                        yield {X = i; Y = j}
        } |> Seq.toList

    {
        Heights = grid.Heights;
        Start = starts;
        End = grid.End;
    }
    

let getNeighbors(pos, grid): Point list =
    let xMax = (grid.Heights.GetLength 0) - 1
    let yMax = (grid.Heights.GetLength 1) - 1
    let posHeight = grid.Heights[pos.X, pos.Y]
    seq {
        if pos.X > 0 then
            yield {X = pos.X - 1; Y = pos.Y}
        if pos.Y > 0 then
            yield {X = pos.X; Y = pos.Y - 1}
        if pos.X < xMax then
            yield {X = pos.X + 1; Y = pos.Y}
        if pos.Y < yMax then
            yield {X = pos.X; Y = pos.Y + 1}
    } 
    |> Seq.filter(fun point -> grid.Heights[point.X, point.Y] <= posHeight + 1) 
    |> Seq.toList

let findPath(start: Point, grid: Grid): int =
    let queue = Queue()

    let rec findPath(explored: Set<Point>, grid: Grid): int = 
        if queue.Count = 0 then
            -1
        else
            let dist, point = queue.Dequeue()
            let height = grid.Heights[point.X, point.Y]

            if point = grid.End then
                dist
            else
                let neighbors = getNeighbors(point, grid)
                let explored = (explored, neighbors) ||> List.fold(
                    fun explored neighbor -> 
                        if not <| explored.Contains(neighbor) then
                            queue.Enqueue((dist + 1, neighbor))
                            explored.Add(neighbor)
                        else
                            explored
                )

                findPath(explored, grid)
            
    queue.Enqueue((0, start))
    let explored = Set.empty |> Set.add(start)

    findPath(explored, grid)


        

let getDay12Solution =
    printf "\nDAY 12\n"

    let gridStr = readFile @"heightMap.txt" |> Seq.toList
    let grid = parseGrid(gridStr)
    let result = findPath(grid.Start[0], grid)

    let allStartsGrid = findAllStarts(grid)
    let shortestPath = 
        allStartsGrid.Start 
        |> List.map(fun point -> findPath(point, grid))
        |> List.filter(fun dist -> dist <> -1)
        |> List.min

    printfn "%A" result
    printfn "%A" shortestPath
