package leetcode
import org.scalatest.{FunSuite, Matchers}

class WaterJugsTest extends FunSuite with Matchers {
  test("should return true if a quantity can be measured") {
    val result = WaterJugs.isMeasurable(3, 5, 4)
    result should be(true)
    val result2 = WaterJugs.isMeasurable(1, 2, 3)
    result2 should be(true)
    val result3 = WaterJugs.isMeasurable(1, 0, 0)
    result3 should be(true)
  }
  test("should return false if a quantity can't be measured") {
    val result = WaterJugs.isMeasurable(2, 6, 5)
    result should be(false)
  }
}
