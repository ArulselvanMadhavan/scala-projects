package dp

import org.scalatest.{FunSuite, Matchers}

class StairsSpec extends FunSuite with Matchers {
  test("should return the correct number of steps for any positive number") {
    val res1 = Stairs.climbStairs(4)
    res1 should be(5)
    val res2 = Stairs.climbStairs(5)
    res2 should be(8)
    val res3 = Stairs.climbStairs(6)
    res3 should be(13)
    val res4 = Stairs.climbStairs(2)
    res4 should be(2)
  }
}
