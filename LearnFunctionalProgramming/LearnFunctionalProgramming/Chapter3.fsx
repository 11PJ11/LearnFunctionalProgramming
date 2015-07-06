//---------------------
// CHAPTER 3 
//---------------------

module dataStructures

    //Singly linked list
    //Named FPList to avoid Microsoft List
    //type definition
    
    type FPList<'a> = 
        | Nil
        | Cons of 'a * ('a FPList)

    //companion object
    type FPList =

        static member sum(ints: FPList<int>) :int=
            match ints with
            | Nil -> 0
            | Cons(x, xs) -> x + FPList.sum(xs)

        static member product(ds: FPList<double>) :double = 
            match ds with
            | Nil -> 1.0
            | Cons(0.0, _) -> 0.0
            | Cons(x, xs) -> x * (FPList.product xs)   

        //This is like a factiry function that creates FPList<'a> given a list
        static member apply(aas: 'a list) :FPList<'a>=
            if (aas.IsEmpty) 
            then Nil
            else Cons(aas.Head, FPList.apply(aas.Tail))

        //EXERCISE 3.2
        static member tail(aas: FPList<'a>) :FPList<'a> =
            match aas with
            | Nil -> Nil
            | Cons(x, xs) -> xs
        
        //EXERCISE 3.3
        //drops the first n elements from aas
        static member drop(n:int, aas: FPList<'a>) :FPList<'a> =
            let rec go(take: int, count: int, xs:FPList<'a>) =
                if count = take
                then xs
                else 
                    match xs with
                    | Nil -> Nil
                    | Cons(y, ys) -> go(take, count + 1, ys)
            
            go(n, 0, aas)


    //EXERCISE 3.1
    let x = match FPList.apply([1;2;3;4;5]) with           
            | Cons(x, Cons(2, Cons(4, _))) -> x  
            | Nil -> 42  
            | Cons(x, Cons(y, Cons(3, Cons(4, _)))) -> x + y  //this is matched x=1 y=2 
            | Cons(h, t) -> h + FPList.sum(t)  
            | _ -> 101   

    let l = FPList.apply [1;2;3;4;5]
    let dropped = FPList.drop (1, l)