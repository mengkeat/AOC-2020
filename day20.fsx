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

let findTileFitEdge (e: char[]) (currTileNum: int) = 
    let stateEdges (t:Tile) = List.map edges t.States |> List.collect id   
    let edgeMatchTile (t:Tile) = List.contains e (stateEdges t)
    seq {for t in Tiles -> 
            if currTileNum=t.Nb || (edgeMatchTile t |> not) then None else Some(t) 
        } |> Seq.choose id

// Note: unfinished
// let findStateFitEdge (e: char[]) (currTileNum: int) = 
//     let stateEdges (t:Tile) = List.map edges t.States 
//     let edgeMatchTile (t:Tile) = stateEdges t |> List.map (List.contains e)
//     let matches (t:Tile) = List.zip (edgeMatchTile t) t.States |> List.filter fst |> List.map snd
//     seq {for t in Tiles -> 
//             let fittingStates = matches t
//             if currTileNum=t.Nb || fittingStates.IsEmpty then None else Some((t.Nb, fittingStates)) 
//     } |> Seq.choose id |> Seq.head

let nbTileFit currtile =
    let refEdges = edges currtile.States.[0]
    seq { for e in refEdges -> findTileFitEdge e currtile.Nb} |> Seq.collect id |> Seq.length

let cornerTiles = Tiles |> List.choose (fun t-> if (nbTileFit t)=2 then Some(t) else None)
let p1 = cornerTiles |> List.map (fun t-> uint64 t.Nb) |> List.reduce (*)
printfn "Part 1: %A" p1

let TopLeftTile = 
    let isTL (t:Tile) = 
        let e = edges t.States.[0]
        let hasTile edge = (findTileFitEdge edge t.Nb) |> (fun s -> (Seq.length s) >0)
        (hasTile e.[0] |> not) && (hasTile e.[1]) && (hasTile e.[2] |> not ) && (hasTile e.[3])
    cornerTiles |> List.filter isTL |> List.head 

printfn "TL: %A" TopLeftTile.Nb
