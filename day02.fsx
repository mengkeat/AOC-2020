open System
open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day02.txt" ) 

let isValid_count (x: string) =
    let [|a; b; c; s|] = x.Split([|' '; '-'; ':'|], StringSplitOptions.RemoveEmptyEntries) 
    let count: int = Seq.filter ((=) (char c)) s |> Seq.length
    count>=(int a) && count<=(int b) 

let part1 = dat |> Seq.filter isValid_count |> Seq.length

let isValid_pos (x: string) = 
    let [|a; b; c; s|] = x.Split([|' '; '-'; ':'|], StringSplitOptions.RemoveEmptyEntries) 
    let ok = fun p -> if s.[p-1]=(char c) then 1 else 0
    ok (int a) + ok (int b) = 1

let part2 = dat |> Seq.filter isValid_pos |> Seq.length
