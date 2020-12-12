open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day12.txt" ) |> Seq.map (fun x-> (x.[0], x.[1..]|> int ))

type State = { X: int; Y:int; Dirn: int * int }

let rotRight currdir angle = [0..(angle/90)-1] |> Seq.fold (fun (ax, ay) _ -> (ay, -ax)) currdir
let rotLeft currdir angle = [0..(angle/90)-1] |> Seq.fold (fun (ax, ay) _ -> (-ay, ax)) currdir 

let move (s:State) (instr: char*int) = 
    match instr with 
    | ('N', d) -> {s with Y = s.Y + d }
    | ('S', d) -> {s with Y = s.Y - d }
    | ('E', d) -> {s with X = s.X + d }
    | ('W', d) -> {s with X = s.X - d }
    | ('R', a) -> {s with Dirn = rotRight s.Dirn a}
    | ('L', a) -> {s with Dirn = rotLeft s.Dirn a}
    | ('F', a) -> {s with X = s.X + (fst s.Dirn)*a; Y = s.Y + (snd s.Dirn)*a}
    | _ -> s

let part1 = dat |> Seq.fold move {X=0; Y=0; Dirn=(1,0)} |> (fun s -> abs s.X + abs s.Y)

let move2 (s:State, wp: int*int) (instr: char*int) =
    match instr with
    | ('N', d) -> (s, (fst wp, snd wp+d))
    | ('S', d) -> (s, (fst wp, snd wp-d))
    | ('E', d) -> (s, (fst wp+d, snd wp))
    | ('W', d) -> (s, (fst wp-d, snd wp))
    | ('R', a) -> (s, rotRight wp a)
    | ('L', a) -> (s, rotLeft wp a)
    | ('F', a) -> ({s with X = s.X + (fst wp)*a; Y = s.Y + (snd wp)*a}, wp)
    | _ -> (s,wp) 

let part2 = dat |> Seq.fold move2 ({X=0; Y=0; Dirn=(1,0)}, (10,1)) |> (fun s -> abs (fst s).X + abs (fst s).Y)