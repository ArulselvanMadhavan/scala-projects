package hackerrank.strings

import java.util.Scanner;

object Mars {

  private[this] def countLoss(sos: String): Int = {
    ("SOS" zip sos).foldLeft(0) { case (acc, (x, y)) => if (x == y) acc else acc + 1 }
  }

  def countMismatch(s: String): Int = {
    val results = for {
      sos <- s.sliding(3, 3)
    } yield countLoss(sos)
    results.sum
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in);
    val s  = sc.next()
    println(countMismatch((s)))
  }
}
