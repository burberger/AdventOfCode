module MonkeyMiddle

open Utils

type Operand =
    | Old
    | Const of int

type Operation = 
    | Add of Operand * Operand
    | Mult of Operand * Operand

type Monkey = {
    Id: int
    Items: List<int>
    Op: Operation
    Test: int
    Condition: int * int
    EvalCount: int
}

let parseItems(startingItemsStr: string): List<int> =
    let justItems = (startingItemsStr.Split ':')[1]
    justItems.Split ',' |> Array.map int |> Array.toList

let parseOperand(op: string): Operand =
    if op.Contains "old" then
        Operand.Old
    else
        Operand.Const(int op)

let parseOperation(opStr: string): Operation =
    let justOp = (opStr.Split '=')[1]
    let splitChar = if justOp.Contains '*' then '*' else '+'
    let ops = justOp.Split splitChar |> Array.map parseOperand
    match splitChar with
    | '*' -> Operation.Mult(ops[0], ops[1])
    | '+' -> Operation.Add(ops[0], ops[1])
    | _ -> raise (System.ArgumentException("Invalid operation"))

let parseTest(testStr: string): int =
    (testStr.Split "by")[1] |> int
    
let parseConditionTarget(conditionStr: string): int =
    (conditionStr.Split "monkey")[1] |> int

let parseMonkey(monkeyStr: List<string>): Monkey =
    let id = int (monkeyStr[0][7]) - 48
    let startItems = parseItems monkeyStr[1]
    let op = parseOperation monkeyStr[2]
    let test = parseTest monkeyStr[3]
    let target1 = parseConditionTarget monkeyStr[4]
    let target2 = parseConditionTarget monkeyStr[5]

    {
        Id = id;
        Items = startItems;
        Op = op;
        Test = test;
        Condition = (target1, target2)
        EvalCount = 0
    }

let parseMonkeys(monkeyStr: List<string>): Monkey array =
    seq {
        for i in 0..7..monkeyStr.Length do
            yield parseMonkey(monkeyStr[i..i+6])
    } |> Seq.toArray

let evalOperation(item: int, op: Operation): int =
    let opFunc, a, b = 
        match op with
        | Add (a, b) -> ((+), a, b)
        | Mult (a, b) -> ((*), a, b)

    let opResult = 
        match (a, b) with
        | Old, Old -> opFunc item item
        | Old, Const x -> opFunc item x
        | _ -> raise (System.ArgumentException("Invalid operation"))

    opResult

let evalItems(monkey: Monkey, destressFn: int -> int): List<int * int> =
    let evalResults = monkey.Items |> List.map(fun item -> destressFn(evalOperation(item, monkey.Op)))
    let testResults = evalResults |> List.map(
        fun item -> 
            if item % monkey.Test = 0 then 
                fst monkey.Condition 
            else 
                snd monkey.Condition
    )

    (testResults, evalResults) ||> List.zip

let addItem(item: int, monkey: Monkey): Monkey =
    {
        Id = monkey.Id
        Items = monkey.Items @ [item]
        Op = monkey.Op
        Test = monkey.Test
        Condition = monkey.Condition
        EvalCount = monkey.EvalCount
    }

let updateEvals(monkey: Monkey): Monkey =
    {
        Id = monkey.Id
        Items = []
        Op = monkey.Op
        Test = monkey.Test
        Condition = monkey.Condition
        EvalCount = monkey.EvalCount + monkey.Items.Length
    }

let computeRound(monkeys: Monkey array) = 
    for i in 0..monkeys.Length-1 do
        let monkey = monkeys[i]
        let newItems = evalItems(monkey, fun item -> item / 3)
        newItems |> List.iter(fun (index, item) -> monkeys[index] <- addItem(item, monkeys[index]))
        monkeys[i] <- updateEvals(monkey)

let computeLargeRound(monkeys: Monkey array, maxVal: int) = 
    for i in 0..monkeys.Length-1 do
        let monkey = monkeys[i]
        let newItems = evalItems(monkey, fun item -> item % maxVal)
        newItems |> List.iter(fun (index, item) -> monkeys[index] <- addItem(item, monkeys[index]))
        monkeys[i] <- updateEvals(monkey)

let getActivity(monkeys: Monkey array): int array =
    monkeys |> Array.map(fun monkey -> monkey.EvalCount) |> Array.sortDescending

let printState(monkeys: Monkey array) =
    monkeys |> Array.iter(fun monkey -> printf "Monkey %d, Inspected %d: %A\n" monkey.Id monkey.EvalCount monkey.Items)

let getDay11Solution =
    printf "\nDAY 11\n"

    let monkeyStart = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\monkeyDataEx.txt" |> Seq.toList
    let monkeys = parseMonkeys monkeyStart

    for i in 1..20 do
        //printf"\nROUND %d\n" i
        computeRound monkeys
        //printState monkeys

    let topTwo = monkeys |> getActivity |> Array.take 2
    printf "Top 2: %A\n" topTwo
    printf "Monkey business: %d\n" (topTwo[0] * topTwo[1])

    // Part 2
    let monkeys = parseMonkeys monkeyStart
    let maxVal = 96577
    printf "MaxVal %d\n" maxVal
    for i in 1..1000 do
        computeLargeRound(monkeys, maxVal)
        //printState monkeys

    printState monkeys
    let topTwo = monkeys |> getActivity |> Array.take 2 |> Array.map int64
    printf "Top 2: %A\n" topTwo
    printf "Monkey business: %d\n" (topTwo[0] * topTwo[1])
