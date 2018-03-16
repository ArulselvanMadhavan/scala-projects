package hackerrank.recursion
import java.util.Scanner

final case class Stats(start: Int, end: Int)

object Crossword {

  type Board = Array[Array[Char]]

  def findMissingSpots(a: Board, isHorizontal: Boolean): Map[Int, Stats] = {
    var m: Map[Int, Stats] = Map[Int, Stats]()
    for {
      i <- 0 until a.length
    } {
      val start = a(i).indexOf("-")
      val end   = a(i).reverse.indexOf("-")
      val size  = end - start
      if (size > 1) m = m + (size -> Stats(start, end))
    }
    m
  }

  def solveCrossword(a: Board, cities: Array[String]): Board = {
    a
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val a  = Array.ofDim[Char](10, 10);
    for {
      i <- 0 until 9
    } a(i) = sc.next().toCharArray()
    val cities = sc.next().split(";")
    solveCrossword(a, cities)
  }
}
