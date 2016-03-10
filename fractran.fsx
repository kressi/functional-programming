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
    | None -> [int n]
    | Some quotient -> int n::(run program quotient)

let add = run [(5I,3I);(2I,5I)]
let multiply = run [(455I,33I);(11I,13I);(1I,11I);(3I,7I);(11I,2I);(1I,3I)]