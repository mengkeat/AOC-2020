open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day06.txt")

let unionsets = dat |> Seq.fold (fun acc (e:string) -> if e.Length>0 then (Set.union acc.Head (e|>Set.ofSeq))::acc.Tail else Set.empty::acc) [Set.empty]
let part1 = unionsets |> Seq.map Seq.length |> Seq.reduce (+)

let allset = ['a'..'z'] |> Set.ofSeq
let intersectsets = dat |> Seq.fold (fun acc (e:string) -> if e.Length>0 then (Set.intersect acc.Head (e|>Set.ofSeq))::acc.Tail else allset::acc) [allset]
let part2 = intersectsets |> Seq.map Seq.length |> Seq.reduce (+)