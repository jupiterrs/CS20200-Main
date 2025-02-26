/// KAIST CS220 In-Class Activity Project

/// Modify the function `gcd` in such a way that the function computes the
/// greatest common divisor of two 32-bit integers.
let rec gcd a b =
  // match (a, b) with
  // // | (a, 0) -> a
  // // | _ -> gcd b (a % b) 

  match b with 
  | 0 -> a
  | _ -> gcd b (a % b)
  

gcd 360 210
|> printfn "%d"
