package clrs.chapter2.exercises

import org.scalatest.{Matchers, FunSuite}

class BinarySearchTest extends FunSuite with Matchers {
  test("should find the element if it is available") {
    BinarySearch.search(Array(2, 4, 6, 8, 10, 12, 14, 16, 18))(10) should be(
      Some(4))
    BinarySearch.search(Array(2, 4, 6, 8, 10, 12, 14, 16, 18))(18) should be(
      Some(8))
    BinarySearch.search(Array(2, 4, 6, 8, 10, 12, 14, 16, 18))(2) should be(
      Some(0))
    BinarySearch.search(Array(2, 4, 6, 8, 10, 12, 14, 16, 18))(20) should be(
      None)
  }
}
