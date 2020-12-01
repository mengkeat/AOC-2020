open System.IO

let s = File.ReadAllLines( __SOURCE_DIRECTORY__ + "/day01.txt" );;
let dat = Array.map (fun x-> int x) s;;

let pairs = [for x in 0..dat.Length-1 do for y in x..dat.Length-1 -> (dat.[x], dat.[y])]
let part1 = (List.filter (fun (x,y) -> x+y=2020 ) pairs).Head; 