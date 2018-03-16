package hackerrank.videoamp

import org.scalatest.{FunSuite, Matchers}

class OneSpec extends FunSuite with Matchers {
  test("Should retrun true for anagarams ") {
    val res0 = One.anagram_checker("Some", "moSe")
    res0 should be(true)
  }
}
