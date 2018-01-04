package hackerrank.strings

import org.scalatest.{FunSuite, Matchers}

class TwoCharactersSpec extends FunSuite with Matchers {
  test("should remove successive repeating characters") {
    val data   = IndexedSeq('b', 'e', 'a', 'b', 'e', 'e', 'f', 'e', 'a', 'b')
    val result = TwoCharacters.computeLongestChars(true)(data);
    result should be(5)
  }
  test("should remove some characters") {
    val data   = IndexedSeq('a', 'a', 'a')
    val result = TwoCharacters.computeLongestChars(true)(data);
    result should be(0)
  }
  test("should work on single charater") {
    val data   = IndexedSeq('a')
    val result = TwoCharacters.computeLongestChars(true)(data);
    result should be(0)
  }
}
