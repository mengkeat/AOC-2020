open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day09.txt" ) |> Seq.map uint64 |> Seq.toList

let rec isValid (lst: uint64 list) n = 
    if List.isEmpty lst then false
    elif (List.contains (n-lst.Head) lst.Tail) && (n-lst.Head <> lst.Head) then true
    else isValid lst.Tail n

let part1 = List.windowed 26 dat |> Seq.filter (fun l -> isValid l (List.last l) |> not) |> (Seq.head >> Seq.last)

let rec sumTill (acc: uint64 list) (lst: uint64 list) currsum =
    if List.isEmpty lst || currsum>=part1 then (currsum, acc)
    else sumTill (lst.Head::acc) lst.Tail (currsum + lst.Head)
                        
let rec findSum lst = match (lst, sumTill [] lst 0UL) with
                        | ([], _) -> []
                        | (_, (s, l)) when s=part1 -> l 
                        | (b, _) -> findSum b.Tail 

let part2 =
    let s = findSum dat
    (List.max s) + (List.min s)