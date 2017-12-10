package clrs.dynamicProgramming

import org.scalatest.{FunSuite, Matchers}

class CoinChangeTests extends FunSuite with Matchers {
  test("CoinChange should return the minimum number of coins to get the change") {
    val result  = CoinChange.findMinCoins(Array(1, 5, 10, 25))(30)
    val result2 = CoinChange.findMinCoins(Array(1, 5, 10, 25))(29)
    result should be(2)
    result2 should be(5)
  }
}
