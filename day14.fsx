open System.IO
open System

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day14.txt" ) 

type Instr = Mask of array<char> | Mem of int64 * int64

let parseInstr (str: string) = 
    if str.[..3]="mask" then Mask (str.[7..] |> Array.ofSeq)
    else 
        let m = str.[4..str.IndexOf(']')-1]
        Mem (int64 m, int64 str.[str.IndexOf('=')+2..])

let program = dat |> Seq.map (fun s -> parseInstr(s.Trim())) |> List.ofSeq

type State = { Mask: array<char>; Memory: Map<int64, int64>}

let part1Rule (state:State) (loc: int64) (v: int64) = 
    let vstr = Convert.ToString(v, 2).PadLeft(36,'0') |> Array.ofSeq 
    let getBit a b = if b='X' then a else b
    let resStr = Array.map2 getBit vstr state.Mask |> String
    { state with Memory = Map.add loc (Convert.ToInt64(resStr, 2)) state.Memory}

let exe rule (state: State) (instr: Instr) = 
    match instr with 
    | Mask msk -> { state with Mask=msk }
    | Mem (loc, v) -> rule state loc v 

let runprogram rule = program 
                    |> List.fold (exe rule) {Mask=Array.empty; Memory=Map.empty}
                    |> (fun s -> s.Memory |> Map.fold (fun acc _ v -> acc+v) 0L)

let part1 = runprogram part1Rule
printfn "Part1: %d" part1

let rec floatMem (acc: int64 List) (q: array<char> list):  int64 List = 
    if q.IsEmpty then 
        acc
    else    
        let xpos = Array.tryFindIndex (fun a -> a='X') q.Head
        match xpos with 
        | Some(p) -> 
            let e1, e2 = Array.copy q.Head, Array.copy q.Head
            e1.[p] <- '1'
            e2.[p] <- '0' 
            floatMem acc (q.Tail@[e1; e2])
        | None -> 
            let v = Convert.ToInt64(q.Head |> String, 2)
            floatMem (v::acc) q.Tail

let part2Rule (state: State) (loc: int64) (v: int64) = 
    let locstr = Convert.ToString(loc, 2).PadLeft(36,'0') |> Array.ofSeq 
    let locFloatStr = Array.map2 (fun s m -> if m='0' then s else m) locstr state.Mask
    let possibleAddress = floatMem [] [locFloatStr]
    let newmem = possibleAddress |> Seq.fold (fun accmap addr -> Map.add addr v accmap) state.Memory
    {state with Memory = newmem}

let part2 = runprogram part2Rule 
printfn  "Part2: %d" part2