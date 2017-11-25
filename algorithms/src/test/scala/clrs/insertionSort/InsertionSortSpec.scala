package clrs.insertionSort

import org.scalatest.{Matchers, FunSuite}

class InsertionSortTest extends FunSuite with Matchers {
  test("should sort the passed list of integers") {
    InsertionSort.sort(Array(5, 4, 3)) should be(Array(3, 4, 5))
  }
  test("should sort the passed list of String") {
    InsertionSort.sort(Array("Something", "Else", "Arul")) should be(
      Array("Arul", "Else", "Something"))
  }
  test("should single element array") {
    InsertionSort.sort(Array(1)) should be(Array(1))
  }
  test("should sort big array") {
    InsertionSort.sort(Array(8, 3, 2, 6, 4, 9, 2, 1)) should be(
      Array(1, 2, 2, 3, 4, 6, 8, 9))
  }
}
