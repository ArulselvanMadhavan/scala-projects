package hackerrank.strings

import org.scalatest.{FunSuite, Matchers}

class CaesarSpec extends FunSuite with Matchers {
  test("should rotate characters") {
    val result = Caesar.cipher(2, "middle-Outz")
    result should be("okffng-Qwvb")
  }
}
