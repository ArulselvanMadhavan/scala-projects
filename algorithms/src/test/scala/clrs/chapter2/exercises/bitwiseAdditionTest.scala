package clrs.chapter2.exercises

import org.scalatest.{Matchers, FunSuite}

class BitAdditionTest extends FunSuite with Matchers {
  test("should add two numbers with carry bit") {
    BitAddition.add(List(1, 1), List(1, 1)) should be(List(1, 1, 0))
    BitAddition.add(List(1, 1), List(1, 0)) should be(List(1, 0, 1))
    BitAddition.add(List(1, 1), List(0, 1)) should be(List(1, 0, 0))
  }
  test("should add two numbers without carry bit") {
    BitAddition.add(List(0, 0), List(0, 0)) should be(List(0, 0, 0))
    BitAddition.add(List(1, 0), List(0, 1)) should be(List(0, 1, 1))
  }

}
