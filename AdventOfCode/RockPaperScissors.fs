module RockPaperScissors

open Utils

type GameResult =
    | Win = 6
    | Draw = 3
    | Loss = 0

type SelectedPiece =
    | Rock = 1
    | Paper = 2
    | Scissors = 3


let getMoveForResult(opponentMove: SelectedPiece, result: GameResult): SelectedPiece =
    match (opponentMove, result) with
    | (SelectedPiece.Rock, GameResult.Win) -> SelectedPiece.Paper
    | (SelectedPiece.Rock, GameResult.Draw) -> SelectedPiece.Rock
    | (SelectedPiece.Rock, GameResult.Loss) -> SelectedPiece.Scissors
    | (SelectedPiece.Paper, GameResult.Win) -> SelectedPiece.Scissors
    | (SelectedPiece.Paper, GameResult.Draw) -> SelectedPiece.Paper
    | (SelectedPiece.Paper, GameResult.Loss) -> SelectedPiece.Rock
    | (SelectedPiece.Scissors, GameResult.Win) -> SelectedPiece.Rock
    | (SelectedPiece.Scissors, GameResult.Draw) -> SelectedPiece.Scissors
    | (SelectedPiece.Scissors, GameResult.Loss) -> SelectedPiece.Paper
    | _ -> raise (System.ArgumentException("Invalid move"))

type Game = struct 
    val Opponent: SelectedPiece
    val Player: SelectedPiece

    new(opponent, player) = { Opponent = opponent; Player = player }

    member this.result: int = 
        let score = 
            match (this.Opponent, this.Player) with
            // Losses
            | (SelectedPiece.Rock, SelectedPiece.Scissors) -> 0
            | (SelectedPiece.Paper, SelectedPiece.Rock) -> 0
            | (SelectedPiece.Scissors, SelectedPiece.Paper) -> 0
            // Draws
            | (SelectedPiece.Rock, SelectedPiece.Rock) -> 3
            | (SelectedPiece.Paper, SelectedPiece.Paper) -> 3
            | (SelectedPiece.Scissors, SelectedPiece.Scissors) -> 3
            // Wins
            | (SelectedPiece.Rock, SelectedPiece.Paper) -> 6
            | (SelectedPiece.Paper, SelectedPiece.Scissors) -> 6
            | (SelectedPiece.Scissors, SelectedPiece.Rock) -> 6
            | _ -> raise (System.ArgumentException("Invalid move"))

        score + int this.Player
end


let parseDirectGame (gameStr: string): Game =
    let moves = gameStr.Split [|' '|]
    let opponent = 
        match moves[0] with
        | "A" -> Some(SelectedPiece.Rock)
        | "B" -> Some(SelectedPiece.Paper)
        | "C" -> Some(SelectedPiece.Scissors)
        | _ -> None

    let player = 
        match moves[1] with
        | "X" -> Some(SelectedPiece.Rock)
        | "Y" -> Some(SelectedPiece.Paper)
        | "Z" -> Some(SelectedPiece.Scissors)
        | _ -> None

    new Game(opponent.Value, player.Value)

let parseStrategyGame (gameStr: string): Game = 
    let moves = gameStr.Split [|' '|]

    let opponent = 
        match moves[0] with
        | "A" -> Some(SelectedPiece.Rock)
        | "B" -> Some(SelectedPiece.Paper)
        | "C" -> Some(SelectedPiece.Scissors)
        | _ -> None

    let playerResult = 
        match moves[1] with
        | "X" -> Some(GameResult.Loss)
        | "Y" -> Some(GameResult.Draw)
        | "Z" -> Some(GameResult.Win)
        | _ -> None

    let player = getMoveForResult(opponent.Value, playerResult.Value)

    new Game(opponent.Value, player)

let directGuideResults (gameStrs: List<string>): int = 
    gameStrs |> List.map(fun x -> (parseDirectGame x).result) |> List.sum

let getDay2Solution = 
    let gameStrs = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\StrategyGuide.txt" |> Seq.toList
    let directResults = gameStrs |> List.map(fun x -> (parseDirectGame x).result) |> List.sum
    let strategyResults = gameStrs |> List.map(fun x -> (parseStrategyGame x).result) |> List.sum

    printf("\nDAY 2\n")
    printf "%d\n" directResults
    printf "%d\n" strategyResults
