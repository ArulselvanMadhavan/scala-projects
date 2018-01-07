package hackerrank.search

import java.util.Scanner;

final case class Result(count: Int, rem: Int)

object RadioTransmitter {

  def getMinCoverage(a: IndexedSeq[Int], k: Int): Int = {
    val maxCoverage = 2 * k
    val l           = a.sorted
    val initial     = Result(1, maxCoverage)
    val result = (1 until l.length).foldLeft(initial)((acc, idx) => {
      val coverageNeeded = l(idx) - l(idx - 1)
      // println(s"${l(idx)}\t${coverageNeeded}\t${acc}")
      if (acc.rem == 0) Result(acc.count + 1, maxCoverage) //Start a new window
      else if (coverageNeeded <= acc.rem) Result(acc.count, acc.rem - coverageNeeded)
      else Result(acc.count + 1, maxCoverage) //Gap is so wide. Start a new window
    })
    println(s"FinalResult${result}")
    result.count
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val l  = sc.nextInt()
    val k  = sc.nextInt()
    val inputs = for {
      i <- 0 until l
    } yield sc.nextInt()
    println(getMinCoverage(inputs, k))
  }
}
