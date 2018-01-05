package hackerrank.strings

import java.util.Scanner;

final case class Details(prev: String, count: Int)

object SherlockAnagrams {

  private[this] def count(s: String): Map[Char, Int] = {
    s.foldLeft(Map[Char, Int]())((m, c) => {
      val current = m.getOrElse(c, 0)
      m + ((c, current + 1))
    })
  }

  private[this] def isValidAnagram(m1: Map[Char, Int], m2: Map[Char, Int]): Boolean = {
    m1.forall { case (k, v) => v == m2.getOrElse(k, 0) }
  }

  private[this] def checkAnagram(prev:String, next: String): Boolean = {
    val m1 = count(prev)
    val m2 = count(next)
    isValidAnagram(m1, m2)
  }

  private[this] def computeAnagramsInSubstring(s: String)(acc: Int, size: Int): Int = {
    val slices  = s.sliding(size).toArray
    val results = for {
      prev <- 0 until slices.length
      next <- (prev + 1) until slices.length
      if checkAnagram(slices(prev), slices(next))
    } yield prev
    acc + results.length
  }

  def computeAnagrams(s: String): Int = {
    (1 until s.length).foldLeft(0)(computeAnagramsInSubstring(s))
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in);
    for {
      t <- 0 until sc.nextInt()
    } println(computeAnagrams(sc.next()))
  }

}
