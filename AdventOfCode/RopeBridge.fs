module RopeBridge

open Utils

type Move =
    | Left
    | Right
    | Up
    | Down

[<StructuredFormatDisplay("({X},{Y})")>]
type Point = struct
    val X: int
    val Y: int

    new(x: int, y: int) =
        { X = x; Y = y }

    static member Abs(p: Point): Point =
        Point(abs p.X, abs p.Y)

    member this.move(m: Move): Point =
        match m with
        | Left -> Point(this.X - 1, this.Y)
        | Right -> Point(this.X + 1, this.Y)
        | Up -> Point(this.X, this.Y + 1)
        | Down -> Point(this.X, this.Y - 1)

    member this.diff(other: Point): Point =
        Point(this.X - other.X, this.Y - other.Y)

    member this.distance(other: Point): int =
        let diff = abs (this.diff(other))
        if diff.X > diff.Y then
            diff.X
        else
            diff.Y
end

let normalize(p: Point): Point =
    let x = 
        if p.X <> 0 then
            p.X / abs p.X
        else
            0

    let y = 
        if p.Y <> 0 then
            p.Y / abs p.Y
        else
            0

    Point(x, y)


[<StructuredFormatDisplay("H {Head} T {Tail}")>]
type Rope = struct
    val Head: Point
    val Tail: Point

    new(head: Point, tail: Point) =
        { Head = head; Tail = tail }
end

type LongRope = List<Point>

let getNewTailPosition(head: Point, tail: Point): Point =
    if head.distance(tail) > 1 then
        let diff = head.diff(tail)
        let normDiff = normalize diff
        Point(tail.X + normDiff.X, tail.Y + normDiff.Y)
    else
        tail

let applyMove(rope: Rope, move: Move): Rope =
    let newHead = rope.Head.move(move)
    let newTail = getNewTailPosition(newHead, rope.Tail)

    Rope(newHead, newTail)

let expandMove(moveLine: string): seq<Move> =
    let move = moveLine.Split " "
    let count = int move[1]
    match move[0] with
    | "L" -> seq { for _ in 1..count -> Move.Left }
    | "R" -> seq { for _ in 1..count -> Move.Right }
    | "U" -> seq { for _ in 1..count -> Move.Up }
    | "D" -> seq { for _ in 1..count -> Move.Down }
    | _ -> raise (System.ArgumentException("Invalid move string"))

let rec getMoveSeq(moveSet: List<string>): seq<Move> =
    seq {
        if not (moveSet |> List.isEmpty) then
            yield! expandMove(moveSet |> List.head)
            yield! getMoveSeq(moveSet |> List.tail)
    }

let moveRope(moves: seq<Move>): seq<Rope> = 
    let rope = Rope(Point(0, 0), Point(0, 0))
    let ropeStates, _ = (rope, moves) ||> Seq.mapFold(fun rope move -> 
        let newRope = applyMove(rope, move)
        newRope, newRope
    )
    ropeStates

let applyLongRopeMove(rope: LongRope, move: Move): LongRope =
    let rec applyLongRopeMove(movedKnot: Point, rope: LongRope): LongRope =
        match rope with
        | [] -> []
        | head::tail -> 
            let newTail = getNewTailPosition(movedKnot, head)
            newTail :: applyLongRopeMove(newTail, tail)

    let front = (rope |> List.head).move(move)
    front :: applyLongRopeMove(front, rope |> List.tail)

let moveLongRope(moves: seq<Move>): seq<LongRope> =
    let longRope = seq{ for _ in 1..10 do Point(0, 0) } |> Seq.toList
    let ropeStates, _ = (longRope, moves) ||> Seq.mapFold(fun rope move -> 
        let newRope = applyLongRopeMove(rope, move)
        newRope, newRope
    )
    ropeStates


let displayMoves(moves: seq<Move>, states: seq<Rope>) =
    let pairs = (moves, states) ||> Seq.zip
    for move, state in pairs do
        printf "%A H %A T %A\n" move state.Head state.Tail

let displayLongMoves(moves: seq<Move>, states: seq<Point>) =
    let pairs = (moves, states) ||> Seq.zip
    for move, state in pairs do
        printf "%A T %A\n" move state

let getDay9Solution = 
    printf "\nDAY 9\n"

    let lines = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\ropeMoves.txt" |> Seq.toList
    let moves = getMoveSeq(lines)
    let ropeStates = moveRope(moves)
    let longRopeStates = moveLongRope(moves)
    let endChainPoints = longRopeStates |> Seq.map(List.last)

    let uniqueTails = (Set.empty, ropeStates) ||> Seq.fold(fun set state -> set.Add(state.Tail))
    let uniqueLongTails = (Set.empty, endChainPoints) ||> Seq.fold(fun set state -> set.Add(state))

    printf "%d\n" uniqueTails.Count
    printf "%d\n" uniqueLongTails.Count

