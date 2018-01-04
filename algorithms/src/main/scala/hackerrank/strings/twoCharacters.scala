package hackerrank.strings
import java.util.Scanner;
import scala.annotation.tailrec

object TwoCharacters {

  private[this] def countDuplicates(m: Map[Char, Int], cs: (Char, Char)): Map[Char, Int] = {
    val (prev, next) = cs
    if (m.contains(next)) {
      m + ((next, m.get(next).get + 1))
    } else if (prev == next) {
      m + ((next, 2))
    } else m
  }

  private[this] def getCount(onlyDuplicates: Boolean)(m: Map[Char, Int],
                                                      cs: (Char, Char)): Map[Char, Int] = {
    if (onlyDuplicates) countDuplicates(m, cs)
    else {
      val current = m.getOrElse(cs._2, 0)
      m + ((cs._2, current + 1))
    }
  }

  private[this] def runCharCount(onlyDuplicates: Boolean, cs: IndexedSeq[Char]): Map[Char, Int] =
    if (cs.isEmpty) Map[Char, Int]()
    else if (cs.length == 1) Map[Char, Int]((cs.head, 1))
    else (cs zip cs.tail).foldLeft(Map[Char, Int]())(getCount(onlyDuplicates))

  @tailrec
  def computeLongestChars(onlyDuplicates: Boolean)(cs: IndexedSeq[Char]): Int = {
    val charsWithCounts: Map[Char, Int] = runCharCount(onlyDuplicates, cs)
    if (charsWithCounts.size == 2) {
      cs.length
    } else if (charsWithCounts.isEmpty && cs.length != 0) {
      computeLongestChars(false)(cs)
    } else if (charsWithCounts.isEmpty == false) {
      val minChar = charsWithCounts.minBy(_._2)
      computeLongestChars(true)(cs.filterNot(_ == minChar._1))
    } else {
      0
    }

  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    sc.nextInt()
    val chars = sc.next().toCharArray().toIndexedSeq
    println(computeLongestChars(true)(chars))
  }
}
