//---------------------
// CHAPTER 2 
//---------------------
open System

let abs x = 
    if x < 0 then -x
    else x

let formatResult name n f = printf "The %s of %d is %d." name n (f (n))
let formatAbs x = formatResult "absolute value" x abs

let factorial n = 
    let rec go (n, acc) = 
        if n <= 0 then acc
        else go (n - 1, n * acc)
    go (n, 1)

let formatFactorial n = formatResult "factorial" n factorial
let formatIncrement n = formatResult "increment" n (fun x -> x + 1)

//EXERCISE 2.1
let rec fib = 
    function 
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

//POLIMORPHIC FUNCTIONS: Abstracting over types

//Generic bynary search; we are abstracting over the type of the array
//and the comparison function
//Usage example: binarySearch [|1; 2; 3|] 2 (fun (x,y)-> x > y)
let binarySearch (aas : 'a []) (key : 'a) (gt : 'a * 'a -> bool) : int = 
    let rec go (low : int, mid : int, high : int) : int = 
        if low > high 
        then -mid - 1
        else 
            let mid2 = (low + high) / 2
            let x = aas.[mid2]
            let greater = gt (x, key)
            if (not greater && not (gt (key, x))) 
            then mid2
            elif greater 
            then go (low, mid2, mid2 - 1)
            else 
                go (mid2 + 1, mid2, high)
    go (0, 0, aas.Length)

//EXERCISE 2.2
let isSorted (aas : 'a []) (gt : 'a * 'a -> bool) : bool = 
    let rec go (curIdx : int, sorted : bool) = 
        if not sorted
        then false
        elif Array.length aas <= 1
        then true
        else             
            let nextIdx = curIdx + 1
            if nextIdx >= aas.Length - 1
            then
                (sorted && gt (aas.[curIdx], aas.[nextIdx]))   
            else                     
                go (nextIdx, gt (aas.[curIdx], aas.[nextIdx]))
    go (0, true)
