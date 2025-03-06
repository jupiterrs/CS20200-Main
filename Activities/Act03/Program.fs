/// KAIST CS220 In-Class Activity Project
/// Modify the function `gcd` in such a way that the function computes the
/// greatest common divisor of two 32-bit integers.
let rec gcd a b =
    // match (a, b) with
    // | (a, 0) -> a
    // | _ -> gcd b (a % b)

    match b with
    | 0 -> a
    | _ -> gcd b (a % b)


gcd 360 210 |> printfn "%d"

let rec f i max =
    if i > max then 0 else i + f (i + 1) max

f 1 9 |> printfn "%d"

//Use Newton's Method to find the sqrt x
let threshold = 0.001

let square x = x * x
let isGoodEnough guess x = abs (square guess - x) < threshold

let improve guess x = (guess + (x / guess)) / 2.0

let rec newton guess x =
    if isGoodEnough guess x then
        guess
    else
        newton (improve guess x) x

let sqrt x = newton 1.0 x

sqrt 2 |> printfn "%f"

//This is a good implementation, but we want to hide
//the implementation

let sqroot x =
    let threshold = 0.001
    let square x = x * x
    let isGoodEnough guess = abs (square guess - x) < threshold
    let improve guess = (guess + (x / guess)) / 2.0

    let rec newton guess =
        if isGoodEnough guess then guess else newton (improve guess)

    newton 1.0

let s = sqroot 2
