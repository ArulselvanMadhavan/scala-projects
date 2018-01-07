package hackerrank.euler

import java.util.Scanner;

object SubsetSum {

  def uniqueSubsetSum(v: IndexedSeq[Int], k: Int): Int = {
    v.combinations(k).toIndexedSeq.groupBy(_.sum).filter { case (k, v) => v.length == 1 }.keys.sum
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val l  = sc.nextInt()
    val k  = sc.nextInt()
    val inputs = for {
      i <- 0 until l
    } yield sc.nextInt()
    println(uniqueSubsetSum(inputs, k))
  }
}
