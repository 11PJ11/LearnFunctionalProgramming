//---------------------
// CHAPTER 3 
//---------------------

namespace fprogrammig.dataStructures

    //Singly linked list

    //type definition
    type List<'a> = 
        | Nil
        | Cons of 'a * ('a List)

    //companion object
    type List =     
        member this.sum(ints: List<int>) :int=
            match ints with
            | Nil -> 0
            | Cons(x, xs) -> x + this.sum(xs)

        member this.product(ds: List<double>) :double = 
            match ds with
            | Nil -> 1.0
            | Cons(0.0, _) -> 0.0
            | Cons(x, xs) -> x * (this.product xs)   

        member this.apply(aas: 'a list): List<'a> =
            if (aas.IsEmpty) 
            then Nil
            else Cons(aas.Head, this.apply(aas.Tail))