open System.IO
open System.Text.RegularExpressions

let dat = File.ReadAllText ( __SOURCE_DIRECTORY__ + "/day16.txt" ) 
            |> (fun x -> x.Split("\n\n")) |> List.ofArray

let constraints = 
    let parseConstraints (s:string) = 
        let reg = Regex("(.*): (\d*)-(\d*) or (\d*)-(\d*)")
        let m = reg.Match(s.Trim())
        let field = m.Groups.[1].Value
        let [a;b;c;d] = [for i in 2..5 -> (m.Groups.[i].Value |> int)]
        field, (a,b), (c,d)
    dat.[0].Split("\n") |> Seq.map parseConstraints 

let yourticket = dat.[1].[13..].Trim().Split(',') |> Seq.map int 
let nearbytickets = dat.[2].Trim().Split('\n') |> Seq.skip 1 
                    |> Seq.map (fun (s:string) -> s.Trim().Split(',') |> Seq.map int)

let checkconstraint n (name, (a,b), (c,d)) = ((n>=a)&&(n<=b)) || ((n>=c)&&(n<=d))
let allconstraintsNotOK n = constraints |> Seq.forall (fun c -> checkconstraint n c |> not) 

let ticketviolations = nearbytickets |> Seq.map (Seq.filter allconstraintsNotOK) 

let part1 = ticketviolations |> Seq.concat |> Seq.fold (+) 0
printfn "%A" part1

let ticketFields = 
    let goodtickets = Seq.zip ticketviolations nearbytickets 
                        |> Seq.choose (fun (v,tic) -> if (Seq.toList v).Length=0 then Some(tic) else None) 
    let constraintsOKName n = [for (name, a, b) in constraints -> (name, checkconstraint n (name,a,b))] 
                                |> List.choose (fun (n, ok) -> if ok then Some(n) else None) |> Set.ofList
    let candidateConstraints ticket = Seq.map constraintsOKName ticket

    let allset = [for (n, _, _) in constraints -> n] |> Set.ofList
    let mergeListOfSet lst1 lst2 = [for (s1,s2) in (Seq.zip lst1 lst2) -> (Set.intersect s1 s2)]
    let ticketlen = yourticket |> (List.ofSeq >> List.length)
    let candidates = goodtickets |> Seq.map candidateConstraints |> Seq.fold mergeListOfSet [for _ in 1..ticketlen -> allset] |> List.ofSeq

    let rec reduce (cand: Set<string> list) (used: Set<string>) = 
        if used.Count = allset.Count then cand
        else
            let e = List.find (fun (s: Set<string>) -> s.Count=1 && (used-s=used)) cand 
            reduce (List.map (fun (s: Set<string>) -> if s.Count=1 then s else (s-e)) cand) (Set.union e used)
    reduce candidates Set.empty |> List.map (List.ofSeq >> List.head) 

let part2 = List.zip ticketFields (List.ofSeq yourticket) 
            |> List.choose (fun (field, v) -> if field.StartsWith("departure") then Some(int64 v) else None)
            |> List.reduce (*)
printfn "%A" part2