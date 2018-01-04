package hackerrank.strings

import org.scalatest.{FunSuite, Matchers}

class MarsSpec extends FunSuite with Matchers{
  test("should count mismatch in sos"){
    val result = Mars.countMismatch("SOSSPSSQSSOR")
    result should be(3)
  }
}
