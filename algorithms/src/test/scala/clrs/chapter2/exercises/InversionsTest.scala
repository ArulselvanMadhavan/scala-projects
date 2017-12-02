package clrs.chapter2.exercises

import org.scalatest.{Matchers, FunSuite}

class InversionsTest extends FunSuite with Matchers {
  test("should count inversions") {
    Inversions.count(List(2, 3, 8, 6, 1)) should be(5)
    Inversions.count(List(4, 3, 8, 7, 9, 1)) should be(7)
    Inversions.count(List(9, 6, 5, 3, 2, 1)) should be(15)
  }
}
