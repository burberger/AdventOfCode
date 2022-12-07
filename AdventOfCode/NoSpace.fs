module NoSpace

open Utils


type File = struct
    val Size: int
    val Name: string

    new(lsLine: string) = 
        let words = lsLine.Split ' '
        { Size = int words[0]; Name = words[1] }
end

type Directory = struct
    val Name: string
    val Parent: Option<Directory>
    val Subdirs: List<Directory>
    val Files: List<File>

    new (name: string, parent: Option<Directory>, subdirs: List<Directory>, files: List<File>) =
        { Name = name; Parent = parent; Subdirs = subdirs; Files = files }
end

let rec processDirectory(lines: seq<string>, dir: Directory): Directory =
    try
        let line = lines |> Seq.find(fun line -> not (line.Contains "$ ls" || line.Contains "dir"))
        
        if line.Contains "cd .." then
            dir
        elif line.Contains "cd" then
            let name = line.Split " " |> Array.last
            let newDir = Directory(name, Some(dir), List.empty, List.empty)

            let updatedDir = Directory(
                dir.Name,
                dir.Parent,
                dir.Subdirs @ [processDirectory(lines, newDir)],
                dir.Files
            )
            processDirectory(lines, updatedDir)
        else
            let updatedDir = Directory(
                dir.Name,
                dir.Parent,
                dir.Subdirs,
                dir.Files @ [File(line)]
            )
            processDirectory(lines, updatedDir)
    with
        | :? System.Collections.Generic.KeyNotFoundException -> dir

let processDirectoryListing(lines: seq<string>): Directory = 
    let _ = lines |> Seq.head
    processDirectory(lines, Directory("/", None, List.empty, List.empty))

let printFile(file: File, prefix: string) =
    printf "%s- %s (file, size=%i)\n" prefix file.Name file.Size

let rec printDirectory(dir: Directory, prefix: string) = 
    printf "%s- %s (dir)\n" prefix dir.Name

    let subPrefix = prefix + "  "
    dir.Subdirs |> List.iter(fun subdir -> printDirectory(subdir, subPrefix))
    dir.Files |> List.iter(fun file -> printFile(file, subPrefix))

let rec getSubDirSize(dir: Directory): int =
    let fileSize = (0, dir.Files) ||> List.fold(fun acc file -> acc + file.Size)
    let subDirSizeTotal = (0, dir.Subdirs) ||> List.fold(fun acc subdir -> acc + getSubDirSize(subdir))
    fileSize + subDirSizeTotal

let rec getSubDirSizes(dir: Directory): List<int> = 
    let dirSizes = [getSubDirSize(dir)]

    dirSizes @ (seq {
        for subdir in dir.Subdirs do
            yield! getSubDirSizes(subdir)
    } |> Seq.toList)


let getDay7Solution = 
    let dirListing = readFile @"C:\Users\bob\source\repos\AdventOfCode\AdventOfCode\dirListing.txt"
    let root = processDirectoryListing(dirListing)
    let sizeList = getSubDirSizes(root)
    let totalSmallDirs = sizeList |> List.filter(fun x -> x < 100000) |> List.sum

    let rootSize = sizeList |> List.head
    let availableSpace = 70000000 - rootSize
    let targetSize = 30000000 - availableSpace
    let dirToRemove = sizeList |> List.filter(fun x -> x > targetSize) |> List.min
    
    printf "\nDAY 7\n"
    printf "%d\n" totalSmallDirs
    printf "root %d\n" rootSize
    printf "available %d\n" availableSpace
    printf "target %d\n" targetSize
    printf "%d\n" dirToRemove
