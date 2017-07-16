/**
  * Created by amadhavan1 on 7/3/17.
  */
object factorial {

  def compute(n:Int):Int = {
    @annotation.tailrec
    def go(n:Int, acc:Int):Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  def main(args: Array[String]): Unit = {
    val x = "Factorial of %d is %d"
    println(x.format(5, compute(5)))
  }
}
