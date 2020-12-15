open System.Collections.Generic

let stmap () =
    let d = Dictionary<int64, int64>()
    [0;6;1;7;2;19;20] |> List.iteri (fun i e -> d.Add(int64 e, int64 i)) |> ignore
    d

let nextNum (num, idx, m: Dictionary<int64, int64>) = 
    let nextnum = if m.ContainsKey(num) then idx-m.[num] else 0L 
    if m.ContainsKey(num) then m.[num]<-idx else m.Add(num, idx)
    Some(nextnum, (nextnum, idx+1L, m))

let part1 = Seq.unfold nextNum (0L, 7L, stmap ())  |> Seq.skip (2020-7-2) |> Seq.head
printfn "%A" part1

let part2 = Seq.unfold nextNum (0L, 7L, stmap ())  |> Seq.skip (30000000-7-2) |> Seq.head 
printfn "%A" part2
