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

let findViewDistance(height: int, trees: int array): int =
    let rec findViewDistance(dist: int, trees: int array): int =
        if Array.head trees >= height || trees.Length = 1 then
            dist
        else
            findViewDistance(dist + 1, Array.tail trees)

    findViewDistance(1, trees)

let findDistanceMatrix(treeMatrix: int[,]): int[,] =
    let size = treeMatrix.GetLength 0
    let viewDist = Array2D.zeroCreate size size

    for i in 1..size-2 do
        for j in 1..size-2 do
            let height = treeMatrix[i, j]
            let distances = 
                findViewDistance(height, treeMatrix[i, ..j-1] |> Array.rev) ::
                findViewDistance(height, treeMatrix[i, j+1..]) ::
                findViewDistance(height, treeMatrix[..i-1, j] |> Array.rev) ::
                [findViewDistance(height, treeMatrix[i+1.., j])]

            let total = distances |> List.reduce(fun a b -> a * b)
            viewDist[i, j] <- total

    viewDist

let findMaxScore(viewMat: int[,]): int =
    let size = viewMat.GetLength 0
    seq {
        for i in 0..size-1 do
            yield viewMat[i, *] |> Array.max
    } |> Seq.max

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

    let treeFile = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\treeFile.txt" |> Seq.toList

    let treeMatrix = buildTreeMatrix treeFile 
    let visibleMatrix = findVisibleTreeMatrix treeMatrix
    let totalVisible = totalVisibleTrees visibleMatrix
    let distanceMatrix = findDistanceMatrix treeMatrix
    let maxScore = findMaxScore distanceMatrix

    //Plot treeMatrix
    //Plot(visibleMatrix |> Array2D.map(fun vis -> if vis then 1 else 0))

    printf "%d\n" totalVisible
    printf "%d\n" maxScore

