//---------------------
// CHAPTER 2 
//---------------------
open System

let factorial n = 
    let rec go (n, acc) = 
        if n <= 0 then acc
        else go (n - 1, n * acc)
    go (n, 1)

// Exercise 1
let rec fib n = 
    match n with
    | 0 -> 0
    | x when x <= 2 -> 1
    | _ -> (fib (n - 1)) + (fib (n - 2))

