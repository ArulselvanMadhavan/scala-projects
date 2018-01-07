package hackerrank.implementation

import java.util.Scanner;

object Grading {

  private[this] def tryRounding(x: Int): Int = {
    if (x < 38) x
    else {
      val (quot, rem) = (x / 5, x % 5)
      if (rem >= 3) 5 * (quot + 1) else x
    }
  }

  def grade(xs: IndexedSeq[Int]): IndexedSeq[Int] = {
    xs.map(tryRounding)
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in);
    for {
      t <- 0 until sc.nextInt()
    } println(tryRounding(sc.nextInt()))
  }
}
