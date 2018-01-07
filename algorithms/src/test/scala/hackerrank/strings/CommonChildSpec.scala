package hackerrank.strings

import org.scalatest.{FunSuite, Matchers}

class CommonChildSpec extends FunSuite with Matchers {
  test("HR#1") {
    val res0 = CommonChild.lcs("HARRY", "SALLY")
    res0 should be(2)
  }
  test("HR#2") {
    val res1 = CommonChild.lcs("AA","BB")
    res1 should be(0)
  }
  test("HR#3") {
    val res2 = CommonChild.lcs("SHINCHAN", "NOHARAAA")
    res2 should be(3)
  }
  test("HR#4") {
    val res3 = CommonChild.lcs("ABCDEF","FBDAMN")
    res3 should be(2)
  }
}
