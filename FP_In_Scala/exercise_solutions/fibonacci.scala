object MyModule {
    def fib(n:Int):Int = {
        @annotation.tailrec
        def helper(n:Int, prev:Int, cur:Int):Int = {
            if (n == 0) prev
            else helper(n - 1, cur, prev + cur)
        }
        helper(n,0,1)
    }

    def main(args: Array[String]):Unit = {
        println(fib(6))
        println(fib(0))
    }
}