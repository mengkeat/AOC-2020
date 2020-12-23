let start = [9;4;2;3;8;7;6;1;5]
let start2 = start @ [10..1000000]

let play cups rounds = 
    let CupsLen = List.length cups
    let lnks = Array.create (cups.Length+1) -1
    for i, j in List.pairwise cups do lnks.[i] <- j
    lnks.[List.last cups] <- cups.Head

    let rec playround curr r = 
        if r=0 then lnks
        else
            let a = lnks.[curr]
            let b = lnks.[a]
            let c = lnks.[b]
            let dest = 
                let rec getdest n = 
                    if List.contains n [a;b;c;] then 
                        if n=1 then getdest CupsLen else getdest (n-1)
                    else n
                getdest (if curr=1 then CupsLen else curr-1)

            lnks.[curr] <- lnks.[c]
            lnks.[c] <- lnks.[dest]
            lnks.[dest] <- a 
            playround lnks.[curr] (r-1)

    playround (List.head cups) rounds

let part1 = play start 100
printfn "Part 1: %A" part1

let part2 = 
    let arr = play start2 10000000
    (uint64 arr.[1]) * (uint64 arr.[arr.[1]])
printfn "Part 2: %A" part2
