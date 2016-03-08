// Sieve of Eratosthenes
// returns list of primes less or equal to n
let primes n =
    let rec sieve = function
        | [] -> []
        | prime::tail ->
            prime::(List.filter (fun x -> x % prime <> 0) tail |> sieve)
    sieve [2..n]