open System.Numerics

let pCard, pDoor = 13135480, 8821721
let transform (b: int) (n:int) = BigInteger.ModPow(BigInteger b, BigInteger n, BigInteger 20201227)

let rec compute (k:int) = 
    let rec getLoop n = 
        if (transform 7 n) = (BigInteger k) then n
        else getLoop (n+1)
    getLoop 1

let loopCard = compute pCard
let loopDoor = compute pDoor

let e1 = transform pDoor loopCard
let e2 = transform pCard loopDoor
printfn "%A %A" e1 e2
