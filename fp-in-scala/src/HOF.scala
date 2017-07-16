/**
  * Created by amadhavan1 on 7/3/17.
  */
object HOF {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val result = "The %s of %d is %d"
    result.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (ordered(as(n + 1), as(n))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("Absolute value", -42, abs))
    println(formatResult("Factorial", 5, factorial))
  }
}
