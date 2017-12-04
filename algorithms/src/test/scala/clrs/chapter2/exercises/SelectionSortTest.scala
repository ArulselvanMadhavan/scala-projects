package clrs.chapter2.exercises

import org.scalatest.{Matchers, FunSuite}

class SelectionSortTest extends FunSuite with Matchers {
  test("should sort the passed list of integers") {
    SelectionSort.sort(Array(5, 4, 3)) should be(Array(3, 4, 5))
  }
  test("should sort the passed list of String") {
    SelectionSort.sort(Array("Something", "Else", "Arul")) should be(
      Array("Arul", "Else", "Something"))
  }
  test("should single element array") {
    SelectionSort.sort(Array(1)) should be(Array(1))
  }
  test("should sort big array") {
    SelectionSort.sort(Array(8, 3, 2, 6, 4, 9, 2, 1)) should be(Array(1, 2, 2, 3, 4, 6, 8, 9))
  }
  test("should not alter sorted array") {
    SelectionSort.sort(Array(1, 2, 3, 4, 5)) should be(Array(1, 2, 3, 4, 5))
  }
  test("should sort last Element") {
    SelectionSort.sort(Array(2, 3, 4, 1)) should be(Array(1, 2, 3, 4))
  }
}
