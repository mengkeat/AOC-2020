let deck1 = [18;19;16;11;47;38;6;27;9;22;15;42;3;4;21;41;14;8;23;30;40;13;35;46;50]
let deck2 = [39;1;29;20;45;43;12;2;37;33;49;32;10;26;36;17;34;44;25;28;24;5;48;31;7]

let rec doround (p1: int list) (p2: int list) = 
    if p1.IsEmpty then (2, p2)
    elif p2.IsEmpty then (1, p1) 
    elif p1.Head > p2.Head then doround (p1.Tail@[p1.Head;p2.Head]) p2.Tail
    else doround p1.Tail (p2.Tail@[p2.Head;p1.Head])

let score res = List.rev res |> List.indexed |> List.fold (fun acc (i, n) -> acc+((i+1)*n) ) 0

let part1 = doround deck1 deck2 |> (fun (_, res) -> score res)
printfn "\nPart 1: %A" part1

let rec recCombat (p1: int list) (p2: int list) (hist: Set<int list * int list>) =
    let p1win = fun () -> recCombat (p1.Tail@[p1.Head;p2.Head]) p2.Tail (Set.add (p1, p2) hist)
    let p2win = fun () -> recCombat p1.Tail (p2.Tail@[p2.Head;p1.Head]) (Set.add (p1, p2) hist)
    if hist.Contains (p1, p2) then (1, p1)
    elif p1.IsEmpty then (2, p2)
    elif p2.IsEmpty then (1, p1)
    elif (p1.Head < (List.length p1)) && (p2.Head < (List.length p2)) then
        let recwin, _ = recCombat (p1.[1..p1.Head]) (p2.[1..p2.Head]) Set.empty
        match recwin with
        | 1 -> p1win()
        | _ -> p2win()
    elif p1.Head > p2.Head then p1win()
    else p2win()

let part2 = recCombat deck1 deck2 Set.empty |> (fun (_, res) -> score res)
printfn "Part 2: %A" part2