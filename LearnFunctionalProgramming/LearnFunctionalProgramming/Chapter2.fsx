//---------------------
// CHAPTER 2 
//---------------------
open System

let abs x =
    if x < 0
    then -x
    else x

let formatResult name n f = 
    printf "The %s of %d is %d." name n (f(n))

let formatAbs x =
    formatResult "absolute value" x abs

let factorial n = 
    let rec go (n, acc) = 
        if n <= 0 then acc
        else go (n - 1, n * acc)
    go (n, 1)

let formatFactorial n =
    formatResult "factorial" n factorial

let formatIncrement n = 
    formatResult "increment" n (fun x -> x + 1)

// Exercise 1
let rec fib n = 
    match n with
    | 0 -> 0
    | x when x <= 2 -> 1
    | _ -> (fib (n - 1)) + (fib (n - 2))

