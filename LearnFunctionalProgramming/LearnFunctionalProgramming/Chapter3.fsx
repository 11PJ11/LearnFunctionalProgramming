//---------------------
// CHAPTER 3 
//---------------------
#load "Chapter2.fsx"

open Chapter2.functions

[<AutoOpen>]
module dataStructures = 
    //Singly linked list
    //Named FPList to avoid Microsoft List
    //type definition    
    type FPList<'a> = 
        | Nil
        | Cons of 'a * 'a FPList
    
    //companion object
    type FPList = 
        
        static member foldRight ((l : FPList<'a>), (acc : 'b)) (f : 'a * 'b -> 'b) : 'b = 
            match l with
            | Nil -> acc
            | Cons(x, xs) -> f (x, (FPList.foldRight (xs, acc) f))
        
        static member sum (ints : FPList<int>) : int = 
            FPList.foldRight (ints, 0) (uncurry (+))

        static member product (ds : FPList<double>) : double = 
            FPList.foldRight (ds, 1.0) (uncurry (*))
        
        //This is like a factiry function that creates FPList<'a> given a list
        static member apply (l : 'a list) : FPList<'a> = 
            if (l.IsEmpty) then Nil
            else Cons(l.Head, FPList.apply (l.Tail))
        
        //EXERCISE 3.2
        static member tail (aas : FPList<'a>) : FPList<'a> = 
            match aas with
            | Nil -> Nil
            | Cons(_, xs) -> xs
        
        //EXERCISE 3.3
        //drops the first n elements from aas
        static member drop (n : int) (l : FPList<'a>) : FPList<'a> = 
            let rec go (take : int) (count : int) (xs : FPList<'a>) = 
                match xs with
                | Nil -> Nil
                | Cons(_, ys) -> 
                    if count = take then xs
                    else go take (count + 1) ys
            go n 0 l
        
        //EXERCISE 3.4
        //removes elements from the prefix as long as they match the predicate
        static member dropWhile (predicate : 'a -> bool) (l : FPList<'a>) : FPList<'a> = 
            let rec go (pred : 'a -> bool, xs : FPList<'a>) = 
                match xs with
                | Nil -> Nil
                | Cons(y, ys) -> 
                    if pred y then go (pred, ys)
                    else xs
            go (predicate, l)
        
        //EXERCISE 3.5
        static member setHead (newHead : 'a) (l : FPList<'a>) : FPList<'a> = 
            let tail = FPList.drop 1 l
            Cons(newHead, tail)
        
        static member append (l1 : FPList<'a>) (l2 : FPList<'a>) : FPList<'a> = 
            match l1 with
            | Nil -> l2
            | Cons(h, t) -> Cons(h, (FPList.append t l2))
        
        //EXERCISE 3.6
        static member dropLast (l : FPList<'a>) : FPList<'a> = 
            let rec go (aas : FPList<'a>) (res : FPList<'a>) : FPList<'a> = 
                match aas with
                | Nil -> Nil
                | Cons(_, Nil) -> res
                | Cons(h1, Cons(_, Nil)) -> Cons(h1, Nil)
                | Cons(h, t) -> Cons(h, go t res)
            go l Nil
        
        //EXERCISE 3.9
        static member length (l : FPList<'a>) : int = 
            FPList.foldRight (l, 0) (fun (_, acc) -> acc + 1)
    
    //EXERCISE 3.1
    let x = 
        match FPList.apply ([ 1; 2; 3; 4; 5 ]) with
        | Cons(x, Cons(2, Cons(4, _))) -> x
        | Nil -> 42
        | Cons(x, Cons(y, Cons(3, Cons(4, _)))) -> x + y //this is matched x=1 y=2 
        | Cons(h, t) -> h + FPList.sum (t)
        | _ -> 101
    
    //TESTS
    let l = FPList.apply [ -1; -2; 3; 4; 5 ]
    let summed = FPList.sum l
    let multiplied = FPList.product (FPList.apply [ 2.0; 3.0; 4.0 ])
    let dropped = FPList.drop 3 l
    let noNegativesInFront = FPList.dropWhile (fun x -> x < 0) l
    let newL = FPList.setHead 9 l
    let appended = FPList.append l newL
    let inited = FPList.dropLast l
    let len = FPList.length l
