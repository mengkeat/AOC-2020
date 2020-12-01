open System.IO

let dat = File.ReadAllLines( __SOURCE_DIRECTORY__ + "/day01.txt" ) |> Array.map int;;

let pairs = [for x in 0..dat.Length-2 do for y in x+1..dat.Length-1 -> (dat.[x], dat.[y])]
let part1 = (List.filter (fun (x,y) -> x+y=2020 ) pairs)
                |> (fun x -> 
                   match x.Head with
                   | (a, b) -> (a*b)
                   | _ -> -1)

let threes = [for x in 0..dat.Length-3 do 
                for y in x+1..dat.Length-2 do
                    for z in y+1..dat.Length-1 -> (dat.[x], dat.[y], dat.[z]) ];;
let part2 = List.filter (fun (x,y,z) -> x+y+z=2020) threes
            |> (fun x ->
                match x.Head with 
                | (a,b,c) -> (a*b*c)
                | _ -> -1)