open System.IO

let dat = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/day03.txt")
let cols = Seq.length dat.[0]

let count right down =
        dat 
        |> Seq.indexed 
        |> Seq.filter (fun (r, row) -> r%down=0 && row.[(r*right)/down%cols]='#')
        |> Seq.length
        |> int64

let part1 = count 3 1
let part2 = (count 1 1) * part1 * (count 5 1) * (count 7 1) * (count 1 2)
