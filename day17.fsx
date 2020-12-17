open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day17.txt" ) 
            |> Seq.mapi (fun r row -> Seq.mapi (fun c ch -> ([r; c; 0], ch) ) row )
            |> Seq.collect id
            |> Map.ofSeq

let prod l1 l2 = l1 |> List.map (fun x -> [for e in l2 -> x::e]) |> List.collect id

let neighs dim = [1..dim-1] 
                |> List.fold (fun acc _ -> prod [-1;0;1] acc) [[-1]; [0]; [1]] 
                |> List.filter ((<>) [for _ in 1..dim -> 0])

let mincoord va vb = List.map2 min va vb 
let maxcoord va vb = List.map2 max va vb
let add va vb = List.map2 (+) va vb

let low dim m = Map.fold (fun acc k v  -> mincoord acc k) [for _ in 1..dim -> 0] m 
                |> List.map (fun x -> x-1)
let high dim m = Map.fold (fun acc k v  -> maxcoord acc k) [for _ in 1..dim -> 0] m 
                |> List.map (fun x -> x+1)

let neighCoord dim coord = neighs dim |> List.map (add coord)

let neighActive dim coord (m: Map<int list, char>) = 
    neighCoord dim coord 
    |> List.choose (fun c -> m.TryFind(c)) 
    |> List.filter ((=) '#') 
    |> List.length

let nextval dim coord (m: Map<int list, char>) = 
    let ncount = neighActive dim coord m
    match m.TryFind(coord) with 
    | Some('#') -> if ncount=2 || ncount=3 then '#' else '.'
    | _ -> if ncount=3 then '#' else '.'

let step dim (m: Map<int list, char>) = 
    let itv = List.map2 (fun x y -> [x..y]) (low dim m) (high dim m) 
    List.foldBack prod itv [[]] |> List.fold (fun acc c -> Map.add c (nextval dim c m) acc) Map.empty

let cycle6 dim m = [1..6] 
                |> Seq.fold (fun acc _ -> step dim acc) m
                |> Map.fold (fun acc k v -> if v='#' then acc+1 else acc) 0

printfn "Part 1: %A" (cycle6 3 dat)

let dat4D = dat |> Map.toList  |> List.map (fun (k,v) -> (k@[0], v)) |> Map
printfn "Part 2: %A" (cycle6 4 dat4D)
