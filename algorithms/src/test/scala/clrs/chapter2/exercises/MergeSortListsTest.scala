package clrs.chapter2.exercises

import org.scalatest.{Matchers, FunSuite}

class MergeSortListsTest extends FunSuite with Matchers {
  test("should sort the passed list of integers") {
    MergeSortLists.sort(List(5, 4, 3)) should be(List(3, 4, 5))
  }
  test("should sort the passed list of String") {
    MergeSortLists.sort(List("Something", "Else", "Arul")) should be(
      List("Arul", "Else", "Something"))
  }
  test("should single element array") {
    MergeSortLists.sort(List(1)) should be(List(1))
  }
  test("should sort big array") {
    MergeSortLists.sort(List(8, 3, 2, 6, 4, 9, 2, 1)) should be(
      List(1, 2, 2, 3, 4, 6, 8, 9))
  }
  test("should not alter sorted array") {
    MergeSortLists.sort(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4, 5))
  }
  test("should sort last Element") {
    MergeSortLists.sort(List(2, 3, 4, 1)) should be(List(1, 2, 3, 4))
  }
}
