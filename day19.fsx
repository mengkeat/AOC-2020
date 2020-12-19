open System.IO

let dat = File.ReadAllText ( __SOURCE_DIRECTORY__ + "/day19.txt" )
let sdat = dat.Split('\n')  |> Seq.map (fun s -> s.Trim()) |> Seq.toList
let rulestr = List.takeWhile (fun (s:string) -> s.Length>0) sdat
let tests = List.skip (rulestr.Length+1) sdat

type Rule = | Elem of char | Prod of int list list
type RuleMap = Map<int, Rule>

let addrule (rmap: RuleMap) (str:string) = 
    let k = str.[..str.IndexOf(':')-1] |> int
    let prod = if str.[str.IndexOf(':')+2]='"' then
                    Elem (str.[str.IndexOf(":")+3] |> char)
                else
                str.[str.IndexOf(":")+2..].Split('|') |> Array.toList
                |> Seq.map (fun s -> s.Split() |> Array.toList |> List.filter (fun x -> x.Length>0) |> List.map int)
                |> List.ofSeq
                |> Prod
    Map.add k prod rmap

let AllRules = rulestr |> List.fold addrule Map.empty

let rec rulematch (rules: RuleMap) (rulecand: int list) (str:string) = 
    if rulecand.IsEmpty then str.Length=0
    else if str.Length=0 then rulecand.IsEmpty
    else
        match rules.[rulecand.Head] with
        | Elem a -> (a=str.[0]) && (rulematch rules rulecand.Tail str.[1..])
        | Prod rulelist  -> [for r in rulelist -> (rulematch rules (r@rulecand.Tail) str)] |> List.reduce (||) 

let part1 = tests |> List.map (rulematch AllRules [0])  |> List.filter id |> List.length
printfn "Part 1: %A" part1

let AllRules2 = AllRules |> Map.toList  
                |> List.filter (fun (k,v) -> (k<>8) && (k<>11)) 
                |> List.append [(8, Prod [[42]; [42; 8]])]
                |> List.append [(11, Prod [[42;31]; [42;11;31]])]
                |> Map.ofSeq
let part2 = tests |> List.map (rulematch AllRules2 [0])  |> List.filter id |> List.length
printfn "Part 2: %A" part2