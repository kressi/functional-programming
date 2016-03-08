// FRACTRAN
// https://de.wikipedia.org/wiki/FRACTRAN
// FRACTRAN is a Turing-complete esoteric programming language invented by the
// mathematician John Conway. A FRACTRAN program is an ordered list of positive
// fractions together with an initial positive integer input n.

type Rational = { Dividend:bigint ; Divisor:bigint }

let rec run (program:List<Rational>) (n:bigint) =

    let rec rmultiply = function
        | [] -> None
        | { Dividend=dividend ; Divisor=divisor }::tail ->
            match bigint.DivRem(n*divisor, dividend) with
            | quotient, remainder when remainder.IsZero -> Some quotient
            | _ -> rmultiply tail

    match rmultiply program with
    | None -> [n]
    | Some quotient -> n::(run program quotient)


run [{Divisor=(bigint 5);Dividend=(bigint 3)};{Divisor=(bigint 2);Dividend=(bigint 5)}] (bigint 72)