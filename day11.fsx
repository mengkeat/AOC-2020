open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day11.txt" ) 

let m = [ for (x, row) in Seq.indexed dat do
            for (y, c) in Seq.indexed row -> ((x,y), c)] |> Map.ofSeq
type neighCount = { empty: int; occupied: int}

let dir8 = [(-1, -1); (0, -1); (1, -1); (1,0); (-1,0); (-1, 1); (0, 1); (1, 1)]
 
let neigh8 x y = dir8 |> List.map (fun d -> (x+(fst d), y+(snd d)))

let addNeighCount nc k = match k with 
                        | 'L' -> {nc with empty = nc.empty+1} 
                        | '#' -> {nc with occupied = nc.occupied+1}
                        | _ -> nc

let neighElem (m: Map<int*int, char>) x y =
    neigh8 x y 
    |> List.filter (fun k -> m.ContainsKey(k))
    |> Seq.map (fun k -> m.[k])
    |> Seq.fold addNeighCount {empty=0; occupied=0}

let nextElem m x y = 
    let ncount = neighElem m x y
    match m.[(x,y)] with 
    | 'L' when ncount.occupied=0  -> '#'
    | '#' when ncount.occupied>=4 -> 'L'
    | a -> a

let nextMap nextElemFunc m = m |> Map.map (fun (x,y) c -> nextElemFunc m x y) 

let rec converge nextElemFunc state = 
    let next = nextMap nextElemFunc state
    if state=next then state else converge nextElemFunc next

let countOccupied m = Map.toList m |> List.filter (fun x -> (snd x)='#') |> List.length

let part1 = m |> converge nextElem |> countOccupied

let rec traceLine (m: Map<int*int, char>) coord dir =
    match Map.tryFind coord m with
    | Some '#' -> '#'
    | Some 'L' -> 'L'
    | Some '.' -> traceLine m ((fst coord)+(fst dir), (snd coord)+(snd dir)) dir
    | None -> '.'

let neighElemLine (m: Map<int*int, char>) x y = 
    dir8
    |> List.map (fun d -> traceLine m (x+(fst d),y+(snd d)) d)
    |> Seq.fold addNeighCount {empty=0; occupied=0}
    
let nextElem2 m x y = 
    let ncount = neighElemLine m x y
    match m.[(x,y)] with 
    | 'L' when ncount.occupied=0  -> '#'
    | '#' when ncount.occupied>=5 -> 'L'
    | a -> a

let part2 = m |> converge nextElem2 |> countOccupied