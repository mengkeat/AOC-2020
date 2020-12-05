open System.IO
open System

let dat = File.ReadAllLines (  __SOURCE_DIRECTORY__ + "/day05.txt")

let value s = Seq.map (fun s -> match s with |'B' |'R' -> '1' |_ -> '0') s  
                |> Array.ofSeq 
                |> (fun x -> Convert.ToInt32(String(x), 2))
let datvals = dat |> Seq.map value |> List.ofSeq

let part1 = List.max datvals

let part2 = List.sort datvals |> List.pairwise |> List.filter (fun (x,y) -> x+1<>y) 
