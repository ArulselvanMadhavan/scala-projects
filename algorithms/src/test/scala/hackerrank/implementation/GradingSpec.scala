package hackerrank.implementation

import org.scalatest.{FunSuite, Matchers}

class GradingSpec extends FunSuite with Matchers {
  test("HR#1") {
    val res0 = Grading.grade(IndexedSeq(73, 67, 38, 33))
    res0 should be(IndexedSeq(75, 67, 40, 33))
  }
}
