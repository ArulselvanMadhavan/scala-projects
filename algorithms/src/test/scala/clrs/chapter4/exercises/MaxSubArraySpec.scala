package clrs.chapter4.exercises

import org.scalatest.{Matchers, FunSuite}

class MaxSubArrayTests extends FunSuite with Matchers {
  test("should work on an increasing array") {
    val (subarr, sum) = MaxSubArray.maxSubArray(Array(1, 3, 5, 7, 9, 11))
    subarr should be(Array(1, 3, 5, 7, 9, 11))
    sum should be(10)
  }
  test("should work on a decreasing array") {
    val (subarr, sum) = MaxSubArray.maxSubArray(Array(11, 9, 7, 5, 3, 1))
    subarr should be(Array())
    sum should be(0)
  }
  test("should work on increasing followed by decreasing array") {
    val (subarr, sum) =
      MaxSubArray.maxSubArray(Array(1, 3, 7, 9, 8, 6, 4, 2, 0))
    subarr should be(Array(1, 3, 7, 9))
    sum should be(8)
  }
  test("should work the subArray is in the middle") {
    val (subarr, sum) =
      MaxSubArray.maxSubArray(Array(3, 2, 1, 4, 6, 8, 7, 5, 0))
    subarr should be(Array(1, 4, 6, 8))
    sum should be(7)
  }
}
