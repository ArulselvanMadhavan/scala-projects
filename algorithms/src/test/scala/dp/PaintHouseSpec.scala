package dp

import org.scalatest.{Matchers, FunSuite}

class PaintHouseSpec extends FunSuite with Matchers {
  test("should choose the minimum painting cost for each house") {
    val a = PaintHouse.minCost(Array(Array(1, 3, 2), Array(0, 8, 4), Array(2, 1, 0)))
    a should be(2)
  }
}
