package hackerrank.recursion

import java.util.Scanner
import utils.MathUtils.largestNthRoot
import utils.FunctionUtils.flip

object PrintSum {
  type Result = List[List[Int]]
  private[this] def findSums(
      xs: IndexedSeq[Int])(start: Int, end: Int, rem: Int): IndexedSeq[Result] = {
    for {
      i        <- start to end
      powValue = xs(i)
      if (powValue <= rem)
    } yield {
      if (powValue == rem) List(i :: Nil)
      else {
        findSums(xs)(i + 1, end, rem - powValue)
          .filterNot(_.isEmpty)
          .foldLeft(Nil: Result)(_ ::: _)
          .map(i :: _)
      }
    }
  }

  def powerSum(x: Int, n: Int): Int = {
    val maxVal  = largestNthRoot(x.toDouble, 1.0 / n)
    val powFunc = (math.pow _).curried
    val powers  = (0 to maxVal).map(_.toDouble).map(flip(powFunc)(n.toDouble)).map(_.toInt)
    val results = findSums(powers)(1, maxVal, x)
    results.filterNot(_.isEmpty).flatten.length
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val x  = sc.nextInt()
    val p  = sc.nextInt()
    println(powerSum(x, p))
  }
}
