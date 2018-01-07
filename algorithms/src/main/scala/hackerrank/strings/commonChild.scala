package hackerrank.strings

import java.util.Scanner;
import utils.TwoDimArray.{printMatrix, findMax};

object CommonChild {

  type TwoDMatrix[T] = Array[Array[T]]

  private[this] def updateLCS(s1: String,
                              s2: String)(a: TwoDMatrix[Int], i: Int, j: Int): TwoDMatrix[Int] = {
    val maxPossible = Seq(i, j).max
    val s1Idx       = i - 1
    val s2Idx       = j - 1
    if (s1(s1Idx) == s2(s2Idx)) {
      val ssLen = a(i - 1)(j - 1) + 1
      a(i)(j) = if (ssLen <= maxPossible) ssLen else maxPossible
    } else {
      val rowLeft = a(i)(j - 1)
      val colUp   = a(i - 1)(j)
      a(i)(j) = Seq(rowLeft, colUp).max
    }
    a
  }

  private[this] def fillMatrix(
      aa: TwoDMatrix[Int],
      f: (TwoDMatrix[Int], Int, Int) => TwoDMatrix[Int]): TwoDMatrix[Int] = {
    var a = aa
    for {
      i <- 1 until a.length
      j <- 1 until a(0).length
    } a = f(a, i, j)
    a
  }

  def lcs(s1: String, s2: String): Int = {
    var a = Array.fill[Int](s1.length + 1, s2.length + 1)(0)
    val f = updateLCS(s1, s2)(_, _, _)
    a = fillMatrix(a, f)
    // printMatrix(a)
    findMax(a)
  }

  def main(args: Array[String]): Unit = {
    val sc   = new Scanner(System.in)
    val str1 = sc.next()
    val str2 = sc.next()
    println(s"${lcs(str1, str2)}")
  }
}
