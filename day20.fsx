open System.IO
open System

printfn "\n\n======="

let dat = File.ReadAllText ( __SOURCE_DIRECTORY__ + "/day20.txt" )
let tilestr = dat.Split("\r\n\r\n") |> Seq.toList

let rotR t = [for c in (Array2D.length1 t)-1 .. -1 .. 0 -> t.[*,c]] |> array2D
let flipRows t = [for r in (Array2D.length1 t)-1 .. -1 .. 0 -> t.[r,*]] |> array2D
let flipCols t = [for r in 0..(Array2D.length1 t)-1 -> t.[r,*] |> Array.rev] |> array2D

type Tile = { Nb: int; States: char[,] list}
let parseTile (s:string) = 
    let tilenum = s.[5..s.IndexOf(':')-1] |> int
    let grid = s.Split("\r\n") |> Array.skip 1  |> array2D
    let rotTiles = [grid; rotR grid; rotR grid |> rotR; rotR grid |> rotR |> rotR]
    let rflip = [rotTiles; List.map flipRows rotTiles] |> List.collect id
    let cflip = [rflip; List.map flipCols rflip] |> List.collect id
    let allstates = cflip |> Set.ofSeq |> Set.toList
    { Nb=tilenum; States=allstates }

let Tiles = tilestr |> List.map parseTile 

// Edges are in order top, bottom, left, right
let edges (state: char[,]) = 
    let d = Array2D.length1 state
    [state.[0,*]; state.[d-1,*]; state.[*,0]; state.[*,d-1]] 

// Note: Assumes that there is always 0 or 1 match for any edge
let findStateFitEdge ((currTileNum, e): int * char[]) = 
    let stateEdges (t:Tile) = List.map edges t.States 
    let edgeMatchTile (t:Tile) = stateEdges t |> List.map (List.contains e)
    let matches (t:Tile) = List.zip (edgeMatchTile t) t.States |> List.filter fst |> List.map snd
    [for t in Tiles -> 
            let fittingStates = matches t
            assert (fittingStates.IsEmpty || fittingStates.Length=1)
            if currTileNum=t.Nb || fittingStates.IsEmpty then None else Some((t.Nb, fittingStates.[0])) 
    ] |> List.choose id |> List.tryExactlyOne

let nbTileFit currtile =
    let refEdges = edges currtile.States.[0]
    seq { for e in refEdges -> findStateFitEdge (currtile.Nb, e) } |> Seq.choose id |> Seq.length

let cornerTiles = Tiles |> List.choose (fun t-> if (nbTileFit t)=2 then Some(t) else None)
let p1 = cornerTiles |> List.map (fun t-> uint64 t.Nb) |> List.reduce (*)
printfn "Part 1: %A" p1
printfn "CornerTiles: %A" (cornerTiles |> List.map (fun t-> t.Nb))

// let TopLeftTile = 
//     let t = cornerTiles.[0]
//     let allEdgeStates = edges t.States.[0]
//     let isTL edges =
//         let hasTile edge = (findStateFitEdge (t.Nb, edge) ) |> (fun s -> match s with | Some(_) -> true | _ -> false)
//         (hasTile e.[0] |> not) && (hasTile e.[1]) && (hasTile e.[2] |> not ) && (hasTile e.[3])
//     allEdgeStates |> List.filter isTL 

// printfn "TL: %A" TopLeftTile

// let tryRightTile ((tnum, state): int * char [,]) = 
//     let rightEdge = (edges state).[3]
//     findStateFitEdge (tnum, rightEdge)

// let tryDownTile ((tnum, state): int * char [,]) = 
//     let rightEdge = (edges state).[1]
//     findStateFitEdge (tnum, rightEdge)

// let getRow st = 
//     let nextr (tnum, state) = match tryRightTile (tnum, state) with | Some(a) -> Some((tnum, a)) | _ -> None
//     let startTile = List.find (fun t -> t.Nb=st) Tiles
//     Seq.unfold nextr (startTile.Nb, startTile.States.[0]) 

// let getTile n = List.find (fun x -> x.Nb=n) Tiles
// printfn "Tilefit for 2591: %A" (nbTileFit (getTile 2591))