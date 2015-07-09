//---------------------
// CHAPTER 2 
//---------------------

[<AutoOpen>]
module functions =

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
    //Generic on the type and on the comparison function
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

    //EXERCISE 2.3
    //Partial application: converts a function of a value and a function of
    //                     N arguments and returns a function of N-1 arguments
    //                     as its result.
    //Usage example: let hello = partial1 "hello " (fun (x, y) -> x + y);;
    //               hello "world!"
    //output:        val it : string = "hello world!"
    let partial1 (x: 'a) (f: 'a * 'b -> 'c) :'b -> 'c =
        fun y -> f (x, y) //x is fixed, y has to be given

    //EXERCISE 2.4
    //Curry: converts a function of N arguments into a function of 
    //          one argument that returns another function as its result.
    //Usage example: let add = fun (x, y) -> x + y;; //val add : x:int * y:int -> int
    //               The add function can only be called providing both x and y.
    //               If I want to define a function that alway add 2 to a number I can't with add.
    //               If I curry the add function like:
    //               let curriedAdd = curry add;; //val curriedAdd : (int -> int -> int)
    //               defining a new function as
    //               let curriedAdd2 = curriedAdd 2;; //val curriedAdd2 : (int -> int)
    //               I can now use this function to add 2 to any number like this:
    //               curriedAdd2 4;;
    //               val it : int = 6
    let curry (f: 'a * 'b -> 'c) : 'a -> ('b -> 'c) = 
        fun x -> fun y -> f (x, y)

    //EXERCISE 2.5
    //Uncurry: reverses the transformation of curry
    //Usage example: let add = uncurry curriedAdd;; //curriedAdd is defined in the previous exercise
    //               //val add : (int * int -> int)
    //Now to use add I have to write add (1,2);; add 1 2 gives and error because is interpreted as
    //((add 1) 2) BUT add has a different type signature
    let uncurry (f: 'a -> 'b -> 'c) : 'a * 'b -> 'c =
        fun (x, y) -> f x y

    //NOTE: given 
    //      let add = fun (x, y) -> x + y;; //val add : x:int * y:int -> int
    //The increment by one function can be written in terms of partial
    //      let inc = partial1 1 add;; //val inc : (int -> int)
    //      inc 2;;
    //      val it : int = 3
    //or in terms of currying
    //      let inc = curry add 1;; //val inc : (int -> int)
    //      inc 2;;
    //      val it : int = 3

    //EXERCISE 2.6
    //Function composition: feeds the output of one function as the input of another function.
    //Usage example: let squaredInc = compose((fun x -> x*x), (fun x -> x + 1));; //val squaredInc : (int -> int)
    //               squaredInc 3;;
    //               val it : int = 16
    let compose ((f: 'b -> 'c) , (g: 'a -> 'b)) : 'a -> 'c =
        fun x -> f(g(x))