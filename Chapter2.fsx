

fun factorial (n:Int) :Int =
    fun rec go(n:Int, acc: Int) :Int =
        if n<=0 
            acc
        else
            go(n-1, n*acc)

    go(n, 1)