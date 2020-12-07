open System.IO
open System.Text.RegularExpressions

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day07.txt" )

let breakline (str:string) = str.Split(" bags contain ")

let parseline (str:string) = 
    let rule = breakline str
    let splitrule lst = lst |> List.chunkBySize 4 |> List.map (fun a -> (int a.[0], a.[1]+" "+a.[2]))
    (rule.[0],  rule.[1].Split() |> List.ofArray |> (fun x -> if x.[0]="no" then [(0,"nothing")] else splitrule x))
    
let allrules = dat |> Seq.map parseline |> Map.ofSeq

let part1 = 
    let rec findShiny bag = 
        if bag="shiny gold" then true
        elif bag="nothing" then false
        else allrules |> Map.find bag |> Seq.map (snd >> findShiny) |> Seq.reduce (||)
    (allrules |> Map.toSeq |> Seq.map fst |> Seq.map findShiny |> Seq.filter (id) |> Seq.length)-1

let part2 = 
    let rec findSum bag = 
        if bag="nothing" then 0
        else 
            let rightRule = allrules |> Map.find bag 
            let typecount =  rightRule |> Seq.map fst 
            let typesum = rightRule |> Seq.map (snd >> findSum)
            let typesumcount = Seq.zip typecount typesum  |> Seq.map (fun x-> (fst x)*(snd x)) |> Seq.reduce (+)
            typesumcount + (typecount |> Seq.reduce (+))
    findSum "shiny gold"