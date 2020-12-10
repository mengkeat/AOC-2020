open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day10.txt" ) |> Seq.map uint64 |> Seq.toList |> List.sort

let pairs = 0UL::dat |> List.windowed 2

let part1 = 
    let v1 = pairs |> List.filter (fun [x;y] -> x+1UL=y) |> List.length
    let v3 = pairs |> List.filter (fun [x;y] -> x+3UL=y) |> List.length
    v1*(v3+1)

let rec count (lst: uint64 List)  (dp: Map<uint64, uint64>) = 
    let getCount k = if dp.ContainsKey k then dp.[k] else 0UL
    let curr = getCount (lst.Head-1UL) + getCount (lst.Head-2UL) + getCount (lst.Head-3UL) 
    if lst.Length=1 then curr else count lst.Tail (Map.add lst.Head curr dp)

let part2 =
    let last = List.max dat + 3UL
    count (dat@[last]) (Map.ofList [(0UL,1UL)])
