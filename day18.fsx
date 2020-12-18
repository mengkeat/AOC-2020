open System.IO

let dat = File.ReadAllLines ( __SOURCE_DIRECTORY__ + "/day18.txt" )

type ATOM = V of uint64 | LEFT | RIGHT | ADD | MUL
let ToAtom s = match s with | "(" -> LEFT | ")" -> RIGHT | "+" -> ADD | "*" -> MUL | a -> V(uint64 a)
let tokenize (s:string) = s.Replace("(", "( ").Replace(")", " )").Split() |> Array.toList |> List.map ToAtom

let PRED1 = [(ADD, 0); (MUL, 0); (LEFT, -1); ] |> Map.ofSeq

let rec RPN (pred: Map<ATOM, int>) (input: ATOM list) (output: ATOM list) (op: ATOM list) =
    if input.IsEmpty then (List.rev output)@op
    else
        match input.Head with
        | V(a) -> RPN pred input.Tail (V(a)::output) op
        | LEFT -> RPN pred input.Tail output (LEFT::op)
        | RIGHT -> 
            let c = List.takeWhile ((<>) LEFT) op
            RPN pred input.Tail (List.rev c@output) (List.skip (1+c.Length) op)
        | ADD | MUL as oper -> 
            let c = List.takeWhile (fun x -> pred.[x] >= pred.[oper]) op
            RPN pred input.Tail (List.rev c @output) (oper::(List.skip (c.Length) op))

let rec eval (expr: ATOM list) (stack: uint64 list) = 
    if expr.IsEmpty then stack.Head
    else
        match expr.Head with
        | V(a) -> eval expr.Tail (a::stack)
        | MUL -> 
            let v = stack.[0] * stack.[1]
            eval expr.Tail (v::(List.skip 2 stack))
        | ADD -> 
            let v = stack.[0]+stack.[1]
            eval expr.Tail (v::(List.skip 2 stack))
        | _ -> 0UL

let part1 = dat |> Seq.map (fun s -> RPN PRED1 (tokenize s) [] []) |> Seq.map (fun e -> eval e []) |> Seq.reduce (+)
printfn "Part1: %A" part1

let PRED2 = [(ADD, 1); (MUL, 0); (LEFT, -1)] |> Map.ofSeq
let part2 = dat |> Seq.map (fun s -> RPN PRED2 (tokenize s) [] []) |> Seq.map (fun e -> eval e []) |> Seq.reduce (+)
printfn "Part2 %A" part2
 