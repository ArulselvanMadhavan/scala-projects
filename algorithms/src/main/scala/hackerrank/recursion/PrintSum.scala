package hackerrank.recursion

import java.util.Scanner
import utils.MathUtils.largestNthRoot
import utils.FunctionUtils.flip

object PrintSum {

  private[this] def findSums(
      xs: IndexedSeq[Int])(start: Int, end: Int, rem: Int): IndexedSeq[List[Int]] = {
    for {
      i        <- start to end
      powValue = xs(i)
      if (powValue <= rem)
    } yield {
      if (powValue == rem) i :: Nil
      else {
        //calc new end
        //i + 1 < end
        findSums(xs)(i + 1, end, rem - powValue).filterNot(_.isEmpty).map(i :: _).toList.flatten
      }
    }
  }

  def powerSum(x: Int, n: Int): Int = {
    val maxVal  = largestNthRoot(x.toDouble, 1.0 / n)
    val powFunc = (math.pow _).curried
    val powers  = (0 to maxVal).map(_.toDouble).map(flip(powFunc)(n.toDouble)).map(_.toInt)
    val results = findSums(powers)(1, maxVal, x)
    results.filterNot(_.isEmpty).length
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val x  = sc.nextInt()
    val p  = sc.nextInt()
    println(powerSum(x, p))
  }
}
