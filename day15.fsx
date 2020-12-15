let stmap = [0;6;1;7;2;19;20] |> List.mapi (fun i e -> (int64 e, int64 i)) |> Map.ofList

let nextNum (num, idx, m) = 
    let nextnum = match Map.tryFind num m with
                    | Some(p) -> idx - p
                    | None -> 0L
    Some(nextnum, (nextnum, idx+1L, m.Add(num, idx)))

let part1 = Seq.unfold nextNum (0L, 7L, stmap)  |> Seq.skip (2020-(Map.count stmap)-2) |> Seq.head
let part2 = Seq.unfold nextNum (0L, 7L, stmap)  |> Seq.skip (30000000-(Map.count stmap)-2) |> Seq.head 
