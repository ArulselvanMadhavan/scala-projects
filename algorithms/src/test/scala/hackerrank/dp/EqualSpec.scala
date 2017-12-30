package hackerrank.dp

import org.scalatest.{FunSuite, Matchers}

class EqualSpec extends FunSuite with Matchers {
  test("should return the minimum number of steps") {
    val res0 = Equal.getMinSteps(Vector(2, 2, 3, 7))
    res0 should equal(2)
  }
  test("should return the minimum number of steps when the input is large") {
    val res1 = Equal.getMinSteps(
      Vector(512, 125, 928, 381, 890, 90, 512, 789, 469, 473, 908, 990, 195, 763, 102, 643, 458,
        366, 684, 857, 126, 534, 974, 875, 459, 892, 686, 373, 127, 297, 576, 991, 774, 856, 372,
        664, 946, 237, 806, 767, 62, 714, 758, 258, 477, 860, 253, 287, 579, 289, 496))
    res1 should equal(5104)
  }
  test("should pass hackerrank testcase#11") {
    val res2 = Equal.getMinSteps(Vector(1, 5, 5))
    res2 should equal(3)
  }
  test("should pass HR testcase#12") {
    val res3 = Equal.getMinSteps(Vector(1, 5, 5, 10, 10))
    res3 should equal(7)
  }
  test("should work if inputs are not sorted") {
    val res4 = Equal.getMinSteps(Vector(5, 8, 2, 9))
    res4 should equal(8)
  }
}
