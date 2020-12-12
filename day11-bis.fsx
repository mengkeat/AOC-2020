open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day11.txt" )  |> array2D
let cols = Array2D.length2 dat 
let rows = Array2D.length1 dat
let inbounds x y = x>=0 && x<rows && y>=0 && y<cols

let dir8 = [(-1, -1); (0, -1); (1, -1); (1,0); (-1,0); (-1, 1); (0, 1); (1, 1)]
let neigh8 x y = dir8 
                |> List.map (fun d -> (x+(fst d), y+(snd d)))
                |> List.filter (fun (x,y) -> inbounds x y)

let neighElem (m: char[,]) x y =
    neigh8 x y |> List.fold (fun acc (x,y) -> acc + (if m.[x,y]='#' then 1 else 0)) 0

let nextElem m x y = 
    let ncount = neighElem m x y
    match m.[x,y] with 
    | 'L' when ncount=0  -> '#'
    | '#' when ncount>=4 -> 'L'
    | a -> a

let nextMap (nextElemFunc: char[,]->int->int->char) (m: char[,]) = 
    [for r in 0..rows-1 do [for c in 0..cols-1 -> (nextElemFunc m r c)]] |> array2D

let rec converge nextElemFunc state = 
    let next = nextMap nextElemFunc state
    if state=next then state else converge nextElemFunc next

let countOccupied (m: char[,])  = 
    seq { for i in 0..rows-1 do for j in 0..cols-1 -> if m.[i,j]='#' then 1 else 0} |> Seq.reduce (+)

let part1 = dat |> converge nextElem |> countOccupied

let rec traceLine (m: char[,]) coord dir =
    let x, y = (fst coord), (snd coord)
    if inbounds x y then 
        if m.[x,y]='.'  then traceLine m (x+(fst dir), y+(snd dir)) dir else m.[x,y]
    else 
        '.'

let neighElemLine (m: char [,]) x y = 
    dir8
    |> List.map (fun d -> traceLine m (x+(fst d), y+(snd d)) d)
    |> Seq.fold (fun acc e -> if e='#' then acc+1 else acc) 0
    
let nextElem2 m x y = 
    let ncount = neighElemLine m x y
    match m.[x,y] with 
    | 'L' when ncount=0  -> '#'
    | '#' when ncount>=5 -> 'L'
    | a -> a

let part2 = dat |> converge nextElem2 |> countOccupied