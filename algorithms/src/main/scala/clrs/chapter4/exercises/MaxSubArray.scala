package clrs.chapter4.exercises
import scala.reflect.ClassTag
import clrs.utils.Bounded
// Max SubArray
// 1. Divide and Conquer: Divide the array at midpoint. Find max subarray in the left. Find max subarray in the right.
// 2. Max subarray can cross over the midpoint. Find the maxsubarray(start, end, sum) that crosses the midpoint.
// 3. Compare the sums of left, right and mid(each has start, end , sum). return the one with max sum
// 4. Base Case: when array size is one it starts and ends at the same index(1,1,value)
// 5. When you combine and return
object MaxSubArray {

  private[this] def findDiffArray[T: Numeric: Bounded: ClassTag](a: Array[T]): Array[T] = {
    val result = (1 until a.length)
      .foldLeft(new Array[T](a.length))((dArray: Array[T], idx: Int) => {
        dArray(idx) = implicitly[Numeric[T]].minus(a(idx), a(idx - 1))
        dArray
      });
    result(0) = implicitly[Numeric[T]].negate(a(0))
    result
  }

  private[this] def helper[T: Numeric: Bounded: Ordering](a: Array[T])(acc: (Int, T, T),
                                                                       idx: Int): (Int, T, T) = {
    val (currentStartIdx, profit, sum): (Int, T, T) = acc
    val newSum: T                                   = implicitly[Numeric[T]].plus(sum, a(idx))
    if (implicitly[Ordering[T]].gt(newSum, profit)) (idx, newSum, newSum)
    else (acc._1, acc._2, sum)
  }

  private[this] def findMaxCrossover[T: Numeric: Bounded](a: Array[T]): (Int, Int, T) = {
    val minValue = implicitly[Bounded[T]].minValue
    val zero     = implicitly[Numeric[T]].zero
    val (start, lsum, _) = (a.length / 2 to 0 by -1)
      .foldLeft((a.length / 2, minValue, zero))(helper(a))
    val (end, rsum, _) = (a.length / 2 + 1 until a.length)
      .foldLeft((a.length / 2 + 1, minValue, zero))(helper(a))
    val total =
      if (lsum == minValue || rsum == minValue) minValue
      else implicitly[Numeric[T]].plus(lsum, rsum)
    (start, end, total)
  }

  private[this] def findMaxIndex[T: Numeric: Bounded](a: Array[T]): (Int, Int, T) = {
    if (a.length == 1) (0, 0, a(0))
    else {
      val result = Seq(findMaxIndex(a.slice(0, a.length / 2)),
                       findMaxIndex(a.slice(a.length / 2, a.length)),
                       findMaxCrossover(a))
      val sums   = result.map(x => x._3)
      val resIdx = sums.indices.maxBy(sums)
      result(resIdx)
    }
  }

  private[this] def stats[T: Numeric: Bounded: ClassTag](a: Array[T]): (Int, Int, T) =
    findMaxIndex(findDiffArray(a))

  def maxSubArray[T: Numeric: Bounded: ClassTag](a: Array[T]): (Array[T], T) = {
    val results = stats(a)
    val zero    = implicitly[Numeric[T]].zero
    results match {
      case (start, end, sum) if (implicitly[Numeric[T]].lteq(sum, zero)) =>
        (Array(), zero)
      case (start, end, sum) if (start == end) =>
        (Array(), sum)
      case (start, end, sum) =>
        (a.slice(start - 1, end + 1), implicitly[Numeric[T]].minus(a(end), a(start - 1)))
    }
  }
}
