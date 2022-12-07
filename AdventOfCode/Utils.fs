module Utils

open System.IO

let readFile(filename: string): seq<string> = 
    new StreamReader(filename) |> Seq.unfold (
        fun sr -> 
            match sr.ReadLine() with
            | null -> None
            | str -> Some(str, sr)
    )

