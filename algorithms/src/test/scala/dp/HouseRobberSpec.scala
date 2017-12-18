package dp

import org.scalatest.{FunSuite, Matchers}

class HouseRobberSpec extends FunSuite with Matchers {
  test("Should steal maxvalue from houses") {
    val a    = Array(1, 4, 2)
    val res1 = HouseRobber.rob(a)
    res1 should be(4)
    val a2   = Array(4, 2, 1, 6, 3, 8, 4, 5, 7, 2, 9, 2, 3, 5)
    val res2 = HouseRobber.rob(a2)
    res2 should be(39)
    val a3   = Array(2, 1, 1, 2)
    val res3 = HouseRobber.rob(a3)
    res3 should be(4)
  }
}
