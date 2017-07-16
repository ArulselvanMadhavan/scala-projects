/**
  * Created by amadhavan1 on 7/3/17.
  */
object Fibonacci {

  def compute(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else go(n - 1, curr, prev + curr)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {

  }
}
