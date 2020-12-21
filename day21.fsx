open System.IO
open System

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day21.txt")

let AllFood = 
    let parse (str:string) = 
        let sep = str.IndexOf("(")
        let igrd = str.[..sep-1].Trim().Split(' ') |> List.ofArray 
        let allergn = str.[sep+10 .. str.Length-2].Split(", ") |> List.ofArray 
        igrd, allergn
    dat |> Seq.map parse |> List.ofSeq

let AllIngrd = AllFood |> List.map (fst >> Set.ofList) |> Set.unionMany |> Set.toList
let AllAllergn = AllFood |> List.map (snd >> Set.ofList) |> Set.unionMany |> Set.toList
let AllSet = AllAllergn |> List.map (fun a -> (a, AllIngrd |> Set.ofList)) |> Map.ofList

let possAllergnToIngrd = 
    let updateFood m (ingrd, allergn) = 
        let cand = allergn |> List.map (fun a -> (a, ingrd |> Set.ofList)) 
        cand |> List.fold (fun accM (a, ingrd) -> Map.add a (Set.intersect accM.[a] ingrd) accM) m
    AllFood |> List.fold updateFood AllSet

let possibleIngrd = possAllergnToIngrd |> Map.toList |> List.map snd |> Set.unionMany

let part1 = 
    let notPoss = Set.difference (AllIngrd |> Set.ofList) possibleIngrd
    let countNot ingrdlst = ingrdlst |> List.filter (fun i-> notPoss.Contains(i) ) |> List.length
    AllFood |> List.map (fst >> countNot) |> List.reduce (+)
printfn "Part 1: %A" part1

let rec elim (cand: (string * Set<string>) list) confirmed  =
    if cand.IsEmpty then confirmed
    else
        let single = cand |> List.find (fun c -> (snd c).Count=1)
        let remain = cand |> List.filter ((<>) single) |> List.map (fun (a, ingrd) -> (a, Set.difference ingrd (snd single)))
        elim remain (single::confirmed)

let part2 = 
    let solved = elim (possAllergnToIngrd |> Map.toList) []
    let sortedLst = solved |> List.sortBy fst |> List.map (fun (_, s) -> s |> Set.toList |> List.head) 
    sortedLst |> List.reduce (fun a b -> a+","+b)
printfn "Part 2: %A" part2