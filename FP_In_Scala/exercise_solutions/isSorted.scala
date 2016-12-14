object MyModule {

    def isSorted[A](as:Array[A], ordered:(A, A) => Boolean):Boolean = {
        @annotation.tailrec
        def loop(cur:Int):Boolean = {
            if (cur >= as.length) true
            else if (ordered(as(cur - 1), as(cur))) false
            else loop(cur + 1)
        }
        loop(1)
    }

    def orderedInt(x:Int, y:Int):Boolean =
        x > y

    def main(args:Array[String]):Unit = {
        println(isSorted(Array(1,2,6,4,5), orderedInt))
    }
}