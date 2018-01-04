package hackerrank.strings

import org.scalatest.{FunSuite, Matchers}

class RichieRichSpec extends FunSuite with Matchers {
  test("should return with a count of the minimum changes for palindrome") {
    val result = RichieRich.minPalindromeSteps(1, "3943")
    result should be("3993")
    val res1 = RichieRich.minPalindromeSteps(2, "092282")
    res1 should be("092290")
    val res2 = RichieRich.minPalindromeSteps(1, "092282")
    res2 should be("-1")
  }
}
