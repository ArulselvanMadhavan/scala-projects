package hackerrank.recursion

import org.scalatest.{FunSuite, Matchers}

class PrintSumSpec extends FunSuite with Matchers {
  test("HR#1") {
    val res0 = PrintSum.powerSum(10, 2)
    res0 should be(1)
  }
  test("HR#2") {
    val res1 = PrintSum.powerSum(100,2)
    res1 should be(3)
  }
  test("HR#3") {
    val res2 = PrintSum.powerSum(100,3)
    res2 should be(1)
  }
  test("HR#4") {
    val res3 = PrintSum.powerSum(400, 2)
    res3 should be(55)
  }
}
