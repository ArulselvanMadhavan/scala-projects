package leetcode

import org.scalatest.{FunSuite, Matchers}

class WaterJugsTest extends FunSuite with Matchers {
  test("should return true if a quantity can be measured") {
    val result = WaterJugs.isMeasurable(3,5,4)
    result should be(true)
  }
  test("should return false if a quantity can't be measured") {
    val result = WaterJugs.isMeasurable(2,6,5)
    result should be(false)
  }
}
