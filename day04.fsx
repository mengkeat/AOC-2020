open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day04.txt") 
let records_string: List<string> = dat |> Seq.fold (fun acc x -> if x.Length>0 then acc.Head+" "+x::acc.Tail else ""::acc) [""]
let records_map = [for record in records_string do
                    [for p in record.Trim().Split(' ') -> (p.Split(':').[0], p.Split(':').[1]) ] |> Map.ofSeq  ]
                    
let allbutcid m = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid";]
                |> List.map (fun k -> Map.containsKey k m) 
                |> List.reduce (&&)

let part1 = records_map |> List.map allbutcid |> List.filter id |> List.length

let btw x a b = (int x) |> (fun x -> x>=a && x <=b)
let v1 m  = btw (Map.find "byr" m |> int) 1920 2020
let v2 m = btw (Map.find "iyr" m |> int) 2010 2020
let v3 m = btw (Map.find "eyr" m |> int) 2020 2030
let v4 m = 
    let h:string = Map.find "hgt" m 
    match h.[h.Length-2..] with
        | "cm" -> btw (h.[..h.Length-3] |> int) 150 193
        | "in" -> btw (h.[..h.Length-3] |> int) 59 76
        | _ -> false
let v5 m = 
    let h: string = Map.find "hcl" m 
    let numalpha = ['a'..'f']@['0'..'9']
    let valid_len = Seq.filter (fun x -> Seq.contains x numalpha) h |> Seq.length
    valid_len=6 && h.[0]='#'
let v6 m = List.contains (Map.find "ecl" m) ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] 
let v7 (m: Map<string, string>) = [for c in (Map.find "pid" m) -> List.contains c ['0'..'9']] |> (fun x-> x.Length=9)
let all_ok m = allbutcid m && v1 m && v2 m && v3 m && v4 m && v5 m && v6 m && v7 m

let part2 = records_map |> List.map all_ok |> List.filter id |> List.length