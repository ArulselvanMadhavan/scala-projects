package hackerrank.strings

import org.scalatest.{FunSuite, Matchers}

class SherlockAnagramsSpec extends FunSuite with Matchers {
  test("Hackerrank #1") {
    val res1 = SherlockAnagrams.computeAnagrams("abba")
    res1 should be(4)
  }
  test("Hackerrank #2") {
    val res2 = SherlockAnagrams.computeAnagrams("abcd")
    res2 should be(0)
  }
  test("Hackerrank #3") {
    val res3 = SherlockAnagrams.computeAnagrams("ifailuhkqq")
    res3 should be(3)
  }
  test("Hackerrank #4") {
    val res4 = SherlockAnagrams.computeAnagrams("hucpoltgty")
    res4 should be(2)
  }
}
