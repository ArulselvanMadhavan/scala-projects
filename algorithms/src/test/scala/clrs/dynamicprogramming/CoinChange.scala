package clrs.dynamicProgramming

import org.scalatest.{FunSuite, Matchers}

class CoinChangeTests extends FunSuite with Matchers {
  test("Coin Change should do something") {
    CoinChange.findMinCoins(Array(1, 5, 10, 25))(30)
  }
}
