package clrs.chapter2.exercises

import org.scalatest.{FunSuite, Matchers}

class SumOfTwoTest extends FunSuite with Matchers {
  test("should find the sum if it exists") {
    SumOfTwo.checkSum(List(2, 3, 4, 1, 6, 8, 7, 9))(16) should be(true)
    SumOfTwo.checkSum(List(2, 3, 4, 1, 6, 8, 7, 9))(100) should be(false)
    SumOfTwo.checkSum(List(2, 3, 4, 1, 6, 8, 7, 9))(14) should be(true)
    SumOfTwo.checkSum(List(2, 2))(4) should be(true)
  }
}
