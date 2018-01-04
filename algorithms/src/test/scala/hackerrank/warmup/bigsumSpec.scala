package hackerrank.warmup

import org.scalatest.{FunSuite, Matchers}

class BigSumSpec extends FunSuite with Matchers {
  test("should compute the sum") {
    val data   = IndexedSeq(1000000001, 1000000002, 1000000003, 1000000004, 1000000005);
    val result = BigSum.computeSum(data);
    result should be(5000000015L)
  }
}
