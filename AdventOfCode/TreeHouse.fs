module TreeHouse

open XPlot.Plotly

open Utils

let buildTreeMatrix(treeFile: List<string>): int[,] =
    let size = treeFile.Length
    let treeMatrix = Array2D.zeroCreate size size
    treeFile |> List.iteri (
        fun i line -> 
            line |> String.iteri (
                fun j char -> treeMatrix[i,j] <- (int char - int '0')
            )
    )

    treeMatrix

let findVisibleFront(trees: int array): bool array = 
    let rec findVisibleFront(tallestTree: int, trees: int array): bool array =
        if trees.Length = 0 then
            [||]
        else
            if Array.head trees > tallestTree then
                Array.concat [| [|true|]; findVisibleFront(Array.head trees, Array.tail trees) |]
            else
                Array.concat [| [|false|]; findVisibleFront(tallestTree, Array.tail trees) |]

    findVisibleFront(-1, trees)

let findVisibleBack(trees: int array): bool array =
    let rec findVisibleBack(tallestTree: int, trees: int array): bool array =
        if trees.Length = 0 then
            [||]
        else
            if trees[trees.Length - 1] > tallestTree then
                Array.concat [| findVisibleBack(trees[trees.Length - 1], trees[0..trees.Length - 2]); [|true|] |]
            else
                Array.concat [| findVisibleBack(tallestTree, trees[0..trees.Length - 2]); [|false|] |]

    findVisibleBack(-1, trees)


let findVisible(trees: int array): bool array =
    let front = findVisibleFront(trees)
    let back = findVisibleBack(trees)

    (front, back) ||> Array.map2(||)

let mergeArray(array: bool array, input: bool array) =
    (array, input) ||> Array.map2(||)


let findVisibleTreeMatrix(treeMatrix: int[,]): bool[,] =
    let size = treeMatrix.GetLength 0
    let visibleMatrix = Array2D.zeroCreate size size

    for i in 0..size-1 do
        let row = findVisible(treeMatrix[i, *])
        let col = findVisible(treeMatrix[*, i])
        visibleMatrix[i, *] <- mergeArray(visibleMatrix[i, *], row)
        visibleMatrix[*, i] <- mergeArray(visibleMatrix[*, i], col)

    visibleMatrix

let totalVisibleTrees(visibleMatrix: bool[,]): int =
    let intMat = visibleMatrix |> Array2D.map(fun vis -> if vis then 1 else 0)
    let size = intMat.GetLength 0
    
    seq {
        for i in 0..size - 1 do
            yield intMat[i, *] |> Array.sum
    } |> Seq.sum

let findViewDistance(visible: bool array): int array =
    let rec findViewDistance(currentDistance: int, visible: bool array): int array =
        if visible.Length = 0 then
            [||]
        else
            if Array.head visible then
                Array.concat [| [|currentDistance|]; findViewDistance(currentDistance + 1, Array.tail visible) |]
            else
                Array.concat [| [|currentDistance|]; findViewDistance(1, Array.tail visible) |]

    let size = visible.Length - 1
    Array.concat [| [|0|]; findViewDistance(1, visible[1..size - 1]); [|0|] |]

let findDistance(visible: bool array): int array =
    let front = findViewDistance(visible)
    let back = findViewDistance(visible |> Array.rev) |> Array.rev

    (front, back) ||> Array.map2(*)

let findViewingDistanceMatrix(visibleMatrix: bool[,]): int[,] =
    let size = visibleMatrix.GetLength 0
    let viewMatrix = Array2D.create size size 1

    for i in 0..size-1 do
        let row = findDistance(visibleMatrix[i, *])
        let col = findDistance(visibleMatrix[*, i])
        viewMatrix[i, *] <- (viewMatrix[i, *], row) ||> Array.map2(*)
        viewMatrix[*, i] <- (viewMatrix[*, i], col) ||> Array.map2(*)

    viewMatrix

let Plot(data) = 
    Heatmap(
        z = data
    )
    |> Chart.Plot
    |> Chart.WithWidth 1024
    |> Chart.WithHeight 1024
    |> Chart.Show


let getDay8Solution = 
    printf "\nDAY 8\n"

    let treeFile = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\treeFileEx.txt" |> Seq.toList
    let treeMatrix = buildTreeMatrix treeFile 
    let visibleMatrix = findVisibleTreeMatrix treeMatrix
    let totalVisible = totalVisibleTrees visibleMatrix

    //Plot treeMatrix
    //Plot(visibleMatrix |> Array2D.map(fun vis -> if vis then 1 else 0))

    printf "%d\n" totalVisible

