package hackerrank.strings

import java.util.Scanner;

final case class Stats(data: Array[Char], count: Int)

object RichieRich {

  private[this] def countPalindromes(acc: Stats, cs: (Int, Int)): Stats = {
    val a          = acc.data
    val k          = acc.count
    val (inc, dec) = cs
    val x          = a(inc)
    val y          = a(dec)
    if (x == y) acc
    else {
      a(dec) = a(inc)
      Stats(a, k + 1)
    }
  }

  def minPalindromeSteps(k: Int, s: String): String = {
    val cs      = s.toCharArray
    val mid     = cs.length / 2
    val inc     = 0 until mid
    val dec     = cs.length - 1 to mid by -1
    val initial = Stats(cs, 0)
    val result  = (inc zip dec).foldLeft(initial)(countPalindromes)
    if (result.count <= k) result.data.mkString
    else "-1"
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in);
    sc.nextInt()
    val k      = sc.nextInt()
    val s      = sc.next()
    val result = minPalindromeSteps(k, s)
    println(result)
  }
}
