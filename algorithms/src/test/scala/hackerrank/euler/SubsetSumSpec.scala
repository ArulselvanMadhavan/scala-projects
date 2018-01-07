package hackerrank.euler

import org.scalatest.{FunSuite, Matchers}

class SubsetSumSpec extends FunSuite with Matchers {
  test("HR#1") {
    val res0 = SubsetSum.uniqueSubsetSum(IndexedSeq(1, 3, 6, 8, 10, 11), 3)
    res0 should be(156)
  }
}
