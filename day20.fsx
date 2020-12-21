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
    (state.[0,*], state.[d-1,*], state.[*,0], state.[*,d-1])

let RIGHT = ((fun (_, _, _, r) -> r), (fun (_, _, l, _) -> l))
let BOTTOM = ((fun (_, b, _, _) -> b), (fun (t, _, _, _) -> t))
let LEFT = ((fun (_, _, l, _) -> l), (fun (_, _, _, r) -> r))
let TOP = ((fun (t, _, _, _) -> t), (fun (_, b, _, _) -> b))

let findStateFit tnum state (currTileEdge, nextTileEdge) = 
    let currEdge = edges state |> currTileEdge
    let stateEdges (t:Tile) = List.map (edges >> nextTileEdge) t.States 
    let edgeMatchTile (t:Tile) = stateEdges t |> List.map ((=) currEdge)
    let matches (t:Tile) = List.zip (edgeMatchTile t) t.States |> List.filter fst |> List.map snd
    [for t in Tiles -> 
            let fittingStates = matches t
            assert (fittingStates.IsEmpty || fittingStates.Length=1)
            if tnum=t.Nb || fittingStates.IsEmpty then None else Some((t.Nb, fittingStates.[0])) 
    ] |> List.choose id |> List.tryExactlyOne

let nbTileFit currtile =
    let statefit = findStateFit currtile.Nb currtile.States.[0]
    [statefit RIGHT; statefit BOTTOM; statefit LEFT; statefit TOP] |> Seq.choose id |> Seq.length

let cornerTiles = Tiles |> List.choose (fun t-> if (nbTileFit t)=2 then Some(t) else None)
let p1 = cornerTiles |> List.map (fun t-> uint64 t.Nb) |> List.reduce (*)
printfn "Part 1: %A" p1

let TopLeftTile = 
    let (t:Tile) = cornerTiles.[0]
    let isTL (state: char[,]) =
        let hasTile dirn = findStateFit t.Nb state dirn |> (fun s -> match s with | Some(_) -> true | _ -> false)
        (hasTile TOP |> not) && (hasTile BOTTOM) && (hasTile LEFT |> not) && (hasTile RIGHT)
    let status = List.map isTL t.States
    List.zip status t.States |> List.filter fst |> List.head |> (fun (_, st) -> (t.Nb, st))

printfn "TL: %A" TopLeftTile

let finalmap = 
    let getRow (st: int * char[,]) = 
        let next (tnum, state) = match findStateFit tnum state RIGHT with | Some(a) -> Some((a, a)) | _ -> None
        st::List.unfold next st
    let nextRow (tnum, state) = match findStateFit tnum state BOTTOM with | Some(a) -> Some((getRow a, a)) | _ -> None
    
    let grid = getRow TopLeftTile::List.unfold nextRow TopLeftTile
    let celldim = (Array2D.length1 (snd grid.[0].[0]))-2
    let dim = grid.Length * celldim 
    let finalgrid = Array2D.create dim dim '.'
    for r,row in List.indexed grid do
        for c, (tnum, state) in List.indexed row do
            Array2D.blit state 1 1 finalgrid (celldim*r) (celldim*c) celldim celldim
    finalgrid

printfn "Finalmap: %A %A" (Array2D.length1 finalmap) (Array2D.length2 finalmap)

let monster = [ "                  # "; 
                "#    ##    ##    ###";
                " #  #  #  #  #  #   "] |> array2D

