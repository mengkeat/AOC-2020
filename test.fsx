

let grid = [for i in 0..5 do for j in 0..5 -> (i,j)] |> Set.ofList

Seq.unfold (fun x -> if x < 13 then Some (x, x+1) else None) 1

let fib =
    (1, 1) // Initial state
    |> Seq.unfold (fun state ->
        if (snd state > 1000) then
            None
        else
            Some(fst state + snd state, (snd state, fst state + snd state)))
    |> List.ofSeq