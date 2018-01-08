package hackerrank.search

import java.util.Scanner;

final case class Result(count: Int, rem: Int, lastPos:Int)

object RadioTransmitter {

  def getMinCoverage(a: IndexedSeq[Int], k: Int): Int = {
    val l = a.sorted
    var i = 0
    var c = 0
    while (i < l.length) {
      c += 1
      var loc = l(i) + k
      while (i < l.length && l(i) <= loc) i += 1
      i -= 1
      loc = l(i) + k
      while (i < l.length && l(i) <= loc) i += 1
    }
    c
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
