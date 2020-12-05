module Solution

open System.Text.RegularExpressions

let regexNumbers (pattern:string) (input:string) =
    let m = Regex.Match(input, pattern)
    if m.Success then
        m.Groups
        |> Seq.toList
        |> List.tail
        |> List.map (fun x -> int x.Value)
        |> Some
    else None

type Position = int * int

type Line =
    | HLine of Position * int
    | VLine of Position * int

let trimStr (x:string) = x.Trim()

let tryParseNumberTriple pattern line =
    match regexNumbers pattern line with
    | Some (a::b::c::_) -> Some (a, b, c)
    | _                 -> None

let tryParseVLine =
    tryParseNumberTriple "x=(\d+), y=(\d+)\.\.(\d+)"
    >> Option.map (fun (x,ystart,yend) -> VLine ((x, ystart), yend-ystart+1))

let tryParseHLine =
    tryParseNumberTriple "y=(\d+), x=(\d+)\.\.(\d+)"
    >> Option.map (fun (y,xstart,xend) -> HLine ((xstart, y), xend-xstart+1))

let parseLine (line:string) =
    [tryParseVLine; tryParseHLine] |> List.pick (fun f -> f line)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map trimStr
    |> List.map parseLine

type Grid = {
    maxy : int
    sand : Position Set
    visited : Position Set }

module Grid =
    let empty10 = {
        maxy = 10
        sand = Set.empty
        visited = Set.empty }
    let empty2 = { empty10 with maxy = 2 }

let move dx dy = function | x, y -> x+dx, y+dy

let visit pos grid = {
    grid with
        visited = grid.visited.Add pos
        sand = grid.sand.Add pos }

module Set =
    let addOption x (s:Set<'a>) = if x = None then s else s.Add(x.Value)

let fillstep spring grid =
    let rec loop depth grid track =
        if depth > 100 then failwith "depth exceeded"
        if track = [] then grid else
        let pos = track.Head
        let left, right, below = move -1 0 pos, move +1 0 pos, move 0 1 pos
        let free pos = not (grid.sand.Contains pos)
        if snd pos >= grid.maxy then grid else
        let next, fill =
            match free left, free below, free right with
            |     _,         true,       _    -> [below], []
            |     true,      _   ,       true -> [left; right], [left; right]
            |     true,      _   ,       _    -> [left], [left]
            |     _,         _,          true -> [right], [right]
            | _                               -> [], [pos]
        loop (depth+1) {
            grid with
                sand    = grid.sand    |> Set.union (fill |> Set)
                visited = grid.visited |> Set.union (next |> Set) }
            (next @ track.Tail)

    loop 0 grid [spring]

let fill grid callback =
    let spring = 500, 0
    let rec loop seen grid =
        let grid' = fillstep spring grid
        let seen' = Set.unionMany [grid'.sand; grid'.visited; seen]
        if seen = seen' then seen else
        callback grid'
        loop seen' { grid' with visited = Set.empty; }
    loop Set.empty grid
    |> Set.count

let generateSand = function
    | HLine ((x, y), n) -> seq { 0..n-1 } |> Seq.map (fun dx -> x+dx, y)
    | VLine ((x, y), n) -> seq { 0..n-1 } |> Seq.map (fun dy -> x, y+dy)

let getMaxy = function
    | HLine ((_, y), _) -> y
    | VLine ((_, y), n) -> y+n-1

let toGrid lines = { 
    sand  = lines |> Seq.collect generateSand |> Set
    maxy  = lines |> Seq.map getMaxy |> Seq.max
    visited = Set.empty }

let demolines = parse "x=495, y=2..7
                       y=7, x=495..501
                       x=501, y=3..7
                       x=498, y=2..4
                       x=506, y=1..2
                       x=498, y=10..13
                       x=504, y=10..13
                       y=13, x=498..504"

let demogrid = demolines |> toGrid
