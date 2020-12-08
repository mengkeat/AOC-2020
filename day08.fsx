open System.IO

let prog = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day08.txt" )
        |> Seq.map (fun x -> (x.Trim().Split().[0], x.Trim().Split().[1] |> int)) |> Seq.toList

type Instruction = string * int

let exeCmd (instr: Instruction list) ip acc = 
    match instr.[ip] with
    | ("acc", x) -> (ip+1, acc+x) 
    | ("jmp", x) -> (ip+x, acc)
    | _ -> (ip+1, acc)

let rec run prog ip acc visited = 
    let ip2, acc2 = exeCmd prog ip acc
    if List.contains ip2 visited then (false, ip2, acc2)
    elif ip2>=prog.Length then (true, ip2, acc2)
    else run prog ip2 acc2 (ip2::visited)

let part1 = run prog 0 0 []

let rec changeProg front (rest: Instruction list)  = 
        if List.isEmpty rest |> not then
            let nextprog = match rest.Head with 
                            | ("jmp", x) -> front@("nop", x)::rest.Tail
                            | ("nop", x) -> front@("jmp", x)::rest.Tail
                            | _ -> front@rest
            nextprog::(changeProg (front@[rest.Head]) rest.Tail)
        else [front]

let part2 = changeProg [] prog
            |> Seq.map (fun p -> run p 0 0 [])
            |> Seq.filter (fun (x,_,_) -> x)
            |> Seq.toList