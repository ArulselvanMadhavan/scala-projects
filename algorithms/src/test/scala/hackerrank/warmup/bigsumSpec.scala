package hackerrank.warmup

import org.scalatest.{FunSuite, Matchers}

class BigSumSpec extends FunSuite with Matchers {
  test("should compute the sum") {
    val data = IndexedSeq(1000000001L,1000000002L,1000000003L,1000000004L,1000000005L);
    val result = BigSum.computeSum(data);
    result should be(5000000015L)
  }
}
