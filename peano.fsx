// Create Natural Numbers from Peano Axioms
// https://en.wikipedia.org/wiki/Peano_axioms

// a naturl number is zero or
// successor of a natural number
type NaturalNumber =
    | Zero
    | Succ of NaturalNumber

// evaluate NaturalNumber to uint
let rec evaluate = function
    | Zero -> 0u
    | Succ n -> 1u + (evaluate n)

// create NaturalNumber from uint
let rec interprete = function
    | 0u -> Zero
    | n -> Succ(interprete (n-1u))

// define sum, x ++ y
let rec ( ++ ) x = function
    | Zero -> x
    | Succ y -> Succ (y ++ x)

// define multiplication, x ** y
let rec ( ** ) x = function
    | Zero -> Zero
    | Succ y -> (x ** y) ++ x

// define factorial, fak n
let rec fak = function
    | Zero -> Succ Zero
    | Succ n -> (Succ n) ** (fak n)


// Create Integers from NaturalNumber
// https://en.wikipedia.org/wiki/Integer#Construction
type Integer = { Minuend:NaturalNumber ; Subtrahend:NaturalNumber }

let embed n = { Minuend=n ; Subtrahend=Zero }

let invert n = { Minuend=n.Subtrahend ; Subtrahend=n.Minuend }

let evaluatei (n:Integer) =
    (evaluate n.Minuend |> int) - (evaluate n.Subtrahend |> int)

let interpreti = function
    | z when z < 0 -> uint32 -z |> interprete |> embed |> invert
    | z -> uint32 z |> interprete |> embed

let add x y = { Minuend=x.Minuend ++ y.Minuend ;
                Subtrahend=x.Subtrahend ++ y.Subtrahend }

let sub x y = add x <| invert y

let mul x y = { Minuend=(x.Minuend**y.Minuend) ++ (x.Subtrahend**y.Subtrahend) ;
                Subtrahend=(x.Minuend**y.Subtrahend) ++ (x.Subtrahend**y.Minuend) }