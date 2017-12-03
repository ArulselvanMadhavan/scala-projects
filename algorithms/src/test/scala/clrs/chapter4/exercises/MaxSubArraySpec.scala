package clrs.chapter4.exercises

import org.scalatest.{Matchers, FunSuite}

class MaxSubArrayTests extends FunSuite with Matchers {
  test("should work on an increasing array") {
    val arr = Array(1,3,5,7,9,11)
    val (subarr, sum) = MaxSubArray.maxSubArray(arr)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray(arr)
    start should be(0)
    end should be(5)
    profit should be(10)
    subarr should be(Array(1, 3, 5, 7, 9, 11))
    sum should be(10)
  }
  test("should work on a decreasing array") {
    val (subarr, sum) = MaxSubArray.maxSubArray(Array(11, 9, 7, 5, 3, 1))
    subarr should be(Array())
    sum should be(0)
  }
  test("should work on increasing followed by decreasing array") {
    val arr =Array(1, 3, 7, 9, 8, 6, 4, 2, 0)
    val (subarr, sum) =
      MaxSubArray.maxSubArray(arr)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray(arr)
    start should be(0)
    end should be(3)
    profit should be(8)
    subarr should be(Array(1, 3, 7, 9))
    sum should be(8)
  }
  test("should work the subArray is in the middle") {
    val arr = Array(3, 2, 1, 4, 6, 8, 7, 5, 0)
    val (subarr, sum) =
      MaxSubArray.maxSubArray(arr)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray(arr)
    start should be(2)
    end should be(5)
    profit should be(7)
    subarr should be(Array(1, 4, 6, 8))
    sum should be(7)
  }
  test("should work when the subArray is of size 2") {
    val arr = Array(3, 2)
    val (subarr, sum) = MaxSubArray.maxSubArray(arr)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray(arr)
    start should be(1)
    end should be(1)
    profit should be(0)
    subarr should be(Array())
    sum should be(0)
  }
  test("should work when the subArray is of size 2 and increasing") {
    val arr = Array(2,3)
    val (subarr, sum) = MaxSubArray.maxSubArray(arr)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray((arr))
    start should be(0)
    end should be(1)
    profit should be(1)
    subarr should be(Array())
    sum should be(1)
  }
  test("should work increasing and decreasing array") {
    val arr = Array(4,2,9,8,10,11,1)
    val (subarr, sum) = MaxSubArray.maxSubArray(arr)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray(arr)
    start should be(1)
    end should be(5)
    profit should be(9)
    subarr should be(Array(2, 9, 8, 10, 11))
    sum should be(9)
  }
  test("should work on large arrays") {
    val arr = Array(4,2,6,1,9,2,0,4,3,2,40,0,43)
    val (start, end, profit) = MaxSubArrayLinear.maxSubArray(arr)
    start should be(6)
    end  should be(12)
    profit should be(43)
  }
}
