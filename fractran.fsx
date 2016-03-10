// FRACTRAN
// https://de.wikipedia.org/wiki/FRACTRAN
// FRACTRAN is a Turing-complete esoteric programming language invented by the
// mathematician John Conway. A FRACTRAN program is an ordered list of positive
// fractions together with an initial positive integer input n.

let rec run (program:List<bigint*bigint>) (n:bigint) =

    let rec rmultiply = function
        | [] -> None
        | (divisor,dividend)::tail ->
            match bigint.DivRem(n*divisor, dividend) with
            | quotient, remainder when remainder.IsZero -> Some quotient
            | _ -> rmultiply tail

    match rmultiply program with
    | None -> n
    | Some quotient -> run program quotient

// Example Program
let example = run [(5I,3I);(2I,5I)]
example 12I

// Program padd takes 2^a * 3^b as input and returns 3^(a+b)
let padd = run [(3I,2I)]
let add a b =
    padd (bigint.Pow(2I, a) * bigint.Pow(3I, b))
    |> (fun n -> bigint.Log n / bigint.Log 3I) |> int 

// Program pmultiply takes 2^a * 3^b as input and returns 5^(a*b)
let pmultiply = run [(455I,33I);(11I,13I);(1I,11I);(3I,7I);(11I,2I);(1I,3I)]
let multiply a b =
    pmultiply (bigint.Pow(2I, a) * bigint.Pow(3I, b))
    |> (fun n -> bigint.Log n / bigint.Log 5I) |> int