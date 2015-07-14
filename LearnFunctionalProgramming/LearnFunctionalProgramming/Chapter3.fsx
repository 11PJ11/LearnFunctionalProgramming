//---------------------
// CHAPTER 3 
//---------------------
#load "Chapter2.fsx"

open Chapter2.functions

module dataStructures = 
    //Singly linked list
    //Named FPList to avoid Microsoft List
    //type definition    
    type FPList<'a> = 
        | Nil
        | Cons of 'a * 'a FPList

    //companion object
    type FPList = 
        static member foldRight ((l: FPList<'a>), (acc: 'b)) (op: 'a * 'b -> 'b) :'b = 
            match l with
            | Nil -> acc
            | Cons(x, xs) -> op(x, (FPList.foldRight (xs, acc) op))

        //EXERCISE 3.10
        static member foldLeft  ((l: FPList<'a>), (acc: 'b)) (op: 'b * 'a -> 'b) :'b =
            match l with
            | Nil -> acc
            | Cons(x, xs) -> FPList.foldLeft (xs, op(acc, x)) op

        //EXERCISE 3.11.1
        static member sumL (ints : FPList<int>) : int = 
            FPList.foldLeft (ints, 0) (uncurry (+))

        //EXERCISE 3.11.2
        static member productL (ds : FPList<double>) : double = 
            FPList.foldLeft (ds, 1.0) (uncurry (*))

        static member sumR (ints : FPList<int>) : int = 
            FPList.foldRight (ints, 0) (uncurry (+))

        static member productR (ds : FPList<double>) : double = 
            FPList.foldRight (ds, 1.0) (uncurry (*))

        //This is like a factory function that creates FPList<'a> given a list
        static member apply (l : 'a list) : FPList<'a> =
            let rec go ((aas: 'a list), acc: 'b) (op: 'b * 'a -> 'b) : 'b =
                match aas with
                | [] -> acc
                | h::t -> go (t, op(acc, h)) op
            go (List.rev l , Nil) (fun (t, h) -> Cons(h, t))

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
                    if pred y 
                    then go (pred, ys)
                    else xs
            go (predicate, l)

        //EXERCISE 3.5
        static member setHead (newHead : 'a) (l : FPList<'a>) : FPList<'a> = 
            let tail = FPList.drop 1 l
            Cons(newHead, tail)

        //EXERCISE 3.14
        static member append (l1 : FPList<'a>) (l2 : FPList<'a>) : FPList<'a> = 
            FPList.foldLeft(l1, l2) (fun (x, y) -> Cons(y, x))

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

        //EXERCISE 3.11.3
        static member lengthL (l: FPList<'a>) : int = 
            FPList.foldLeft (l,0) (fun (acc, _) -> acc + 1)

        //EXERCISE 3.12
        static member reverse (l: FPList<'a>) :FPList<'a> =
            FPList.foldLeft (l, Nil) (fun (t, h) -> Cons(h, t))

        //EXERCISE 3.15
        static member flatten (lls: FPList<FPList<'a>>) : FPList<'a> =
            let revFlattened = FPList.foldLeft (lls, Nil) (fun (x, y) -> FPList.append y x )
            FPList.reverse revFlattened

        //EXERCISE 3.16
        static member add1ToEach (l: FPList<int>) :FPList<int> =
            let revIncred = FPList.foldLeft (l, Nil) (fun (t, h) -> Cons(h + 1, t))
            FPList.reverse revIncred

        //EXERCISE 3.17
        static member fromDoubleToString (l: FPList<double>) :FPList<string> =
            let revStringified = FPList.foldLeft (l, Nil) (fun (t, h) -> Cons(h.ToString(), t ) )
            FPList.reverse revStringified

        //EXERCISE 3.18
        static member map (l: FPList<'a>) (f: 'a -> 'b) :FPList<'b> =
            FPList.reverse(FPList.foldLeft(l, Nil) (fun (t, h) -> Cons(f(h), t)))

        //EXERCISE 3.19
        static member filter (l: FPList<'a>) (f: 'a -> bool) :FPList<'a> =
            FPList.reverse(FPList.foldLeft(l, Nil) (fun (t, h) -> if f(h) then Cons(h, t) else t))

        //EXERCISE 3.20
        static member flatMap (l: FPList<'a>) (f: 'a -> FPList<'b>) :FPList<'b> =
            FPList.flatten( FPList.map l f)

        //EXERCISE 3.21
        static member filterFM (l: FPList<'a>) (f: 'a -> bool) :FPList<'a> =
            FPList.flatMap l (fun x -> if f(x) then Cons(x, Nil) else Nil)

    //EXERCISE 3.1
    let x = 
        match FPList.apply ([ 1; 2; 3; 4; 5 ]) with
        | Cons(x, Cons(2, Cons(4, _))) -> x
        | Nil -> 42
        | Cons(x, Cons(y, Cons(3, Cons(4, _)))) -> x + y //this is matched x=1 y=2 
        | Cons(h, t) -> h + FPList.sumL (t)
        //| _ -> 101 never matched

    //TESTS
    let l = FPList.apply([1..10])
    let rSummed = FPList.sumR (FPList.apply [ -2; -3; 4 ])
    let lSummed = FPList.sumL (FPList.apply([1..50000]))
    let rMultiplied = FPList.productR (FPList.apply [ 2.0; 3.0; 4.0 ])
    let lMultiplied = FPList.productL (FPList.apply [ 2.0; 3.0; 4.0 ])
    let dropped = FPList.drop 3 l
    let noNegativesInFront = FPList.dropWhile (fun x -> x < 0) l
    let newL = FPList.setHead 9 l
    let appended = FPList.append l (FPList.apply([20..-1..11]))
    let inited = FPList.dropLast l
    //let len = FPList.length l;; stack overflow
    //let lenR = FPList.length (FPList.apply([1..50000]));; stack overflow
    let lenL = FPList.lengthL (FPList.apply([1..100000]))
    let rev = FPList.reverse l
    let lls = Cons(Cons(1,Cons(2,Nil)),Cons(Cons(3,Cons(4,Nil)),Nil))
    let flattened = FPList.flatten lls
    let incred = FPList.add1ToEach(FPList.apply([1..10]))
    let stringified = FPList.fromDoubleToString(FPList.apply([1.0;2.0;3.0]))
    let incred1 = FPList.map (FPList.apply([1..10])) (fun x -> x + 1)
    let onlyEven = FPList.filter (FPList.apply([1..10])) (fun x -> x % 2 = 0)
    let flatMapped = FPList.flatMap (Cons(1,Cons(2,Cons(3,Nil)))) (fun i -> Cons(i, Cons(i,Nil)))
    let onlyOddFM = FPList.filterFM (FPList.apply([1..20])) (fun x -> x % 2 = 1)