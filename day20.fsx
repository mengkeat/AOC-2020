open System.IO
open System

printfn "\n\n======="

let dat = File.ReadAllText ( __SOURCE_DIRECTORY__ + "/day20.txt" )
let tilestr = dat.Split("\n\n") |> Seq.toList

let rotR t = [for c in (Array2D.length1 t)-1 .. -1 .. 0 -> t.[*,c]] |> array2D
let flipRows t = [for r in (Array2D.length1 t)-1 .. -1 .. 0 -> t.[r,*]] |> array2D
let flipCols t = [for r in 0..(Array2D.length1 t)-1 -> t.[r,*] |> Array.rev] |> array2D

type Tile = { Nb: int; States: char[,] list}
let parseTile (s:string) = 
    let tilenum = s.[5..s.IndexOf(':')-1] |> int
    let grid = s.Split("\n") |> Array.skip 1  |> array2D
    let rotTiles = [grid; rotR grid; rotR grid |> rotR; rotR grid |> rotR |> rotR]
    let rflip = [rotTiles; List.map flipRows rotTiles] |> List.collect id
    let cflip = [rflip; List.map flipCols rflip] |> List.collect id
    { Nb=tilenum; States=cflip }

let Tiles = tilestr |> List.map parseTile 

// Edges are in order top, bottom, left, right
let edges (state: char[,]) = 
    let d = Array2D.length1 state
    [state.[0,*]; state.[d-1,*]; state.[*,0]; state.[*,d-1]] 

let nbTileFit currtile =
    let refEdges = edges currtile.States.[0]
    let stateEdges (t:Tile) = List.map edges t.States |> List.collect id   
    let edgeMatchTile (e: char[]) (t:Tile) = List.contains e (stateEdges t)
    seq { for e in refEdges do 
        seq {for t in Tiles -> if currtile.Nb=t.Nb then false else edgeMatchTile e t} |> Seq.reduce (||)  }  |> Seq.filter id |> Seq.length

let cornerTiles = Tiles |> List.choose (fun t-> if (nbTileFit t)=2 then Some(t) else None)
let p1 = cornerTiles |> List.map (fun t-> uint64 t.Nb) |> List.reduce (*)
printfn "Part 1: %A" p1

