package leetcode

import org.scalatest.{FunSuite, Matchers}

class SpiralMatrixSpec extends FunSuite with Matchers {
  def fillArrayWithValues(a: Array[Array[Int]]): Array[Array[Int]] = {
    for {
      i <- 0 until a.size
      j <- 0 until a(0).size
    } a(i)(j) = (i * j) + j
    a
  }
  test("Spiral Matrix should print elements in spiral order") {
    var a = Array.fill(5, 4)(0)
    a = fillArrayWithValues(a)
    SpiralMatrix.spiralPrint(a)
    var a2 = Array.fill(10, 2)(0)
    a2 = fillArrayWithValues(a2)
    SpiralMatrix.spiralPrint(a2)
  }
}
