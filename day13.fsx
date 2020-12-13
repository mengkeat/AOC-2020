open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day13.txt" ) 
let eTime = dat.[0] |> uint64
let idxbus = dat.[1].Split(',') |> Seq.mapi (fun i e -> if e="x" then None else Some (uint64 i, uint64 e)) |> Seq.choose id |> Seq.toList

let part1 = idxbus |> Seq.map (fun (_, i) -> (i, i-(eTime%i)))  |> Seq.minBy (fun (id, r) -> r)  |> (fun (x,y) -> x*y)

// Jump dt assumes that the busIDs are all co-primes
let rec sieve (i, dt) (offset, busId) =
    Seq.initInfinite (fun n -> (uint64 n)*dt + i)
    |> Seq.find (fun t -> (t+offset) % busId = 0UL)
    |> fun t -> (t, dt*busId)

let part2 = idxbus |> List.fold sieve (0UL, 1UL) |> fst
