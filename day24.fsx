open System.IO

let dat = File.ReadAllLines (  __SOURCE_DIRECTORY__ + "/day24.txt")  |> Array.toList |> List.map List.ofSeq

let rec decode (str: char list) (x, y) = 
    if str.IsEmpty then (x,y)
    else match str with
            | 'n'::'w'::rest -> decode rest (x, y+1)
            | 'n'::'e'::rest -> decode rest (x-1, y+1)
            | 's'::'w'::rest -> decode rest (x+1, y-1)
            | 's'::'e'::rest -> decode rest (x, y-1)
            | 'e'::rest -> decode rest (x-1, y)
            | 'w'::rest -> decode rest (x+1, y)
            | _ -> (-1,-1)

let coords = dat |> List.map (fun s -> decode s (0,0))
let blacks = (List.groupBy id coords) |> List.filter (fun (_, l) -> l.Length%2=1) |> List.map (fun (c, _) -> c)
printfn "\nPart 1: %A" (List.length blacks)

let neigh = [(0,1); (-1,1); (1,-1); (0,-1); (-1,0); (1,0)]
let xmin, xmax  = (List.minBy fst coords) |> fst, (List.maxBy fst coords) |> fst
let ymin, ymax = (List.minBy snd coords) |> snd, (List.maxBy snd coords) |> snd
let XDIM = xmax-xmin+200 
let YDIM = ymax-ymin+200
let (+) (x1,y1) (x2,y2) = (x1+x2, y1+y2)

let rec flip arr n = 
    if n=0 then arr
    else
        let newArr = Array2D.copy arr
        for i in 1..(Array2D.length1 arr)-2 do
            for j in 1..(Array2D.length2 arr)-2 do
                let ncount = neigh |> List.map (fun n -> (j,i)+n) |> List.filter (fun (c,r) -> arr.[r,c]) |> List.length
                if arr.[i,j]=true && ( (ncount=0)||(ncount>2)) then newArr.[i,j] <- false
                elif arr.[i,j]=false && (ncount=2) then newArr.[i,j] <- true

        flip newArr (n-1)

let part2 = 
    let startArr = Array2D.create YDIM XDIM false
    let newc = blacks |> List.map ((+) (XDIM/2, YDIM/2))
    for (c,r) in newc do
        startArr.[r, c] <- true
    let arr = flip startArr 100
    seq {for r in 0..(Array2D.length1 arr)-1 do 
            for c in 0..(Array2D.length2 arr)-1 do
                yield arr.[r,c] } |> Seq.filter id |> Seq.length

printfn "Part 2: %A" part2