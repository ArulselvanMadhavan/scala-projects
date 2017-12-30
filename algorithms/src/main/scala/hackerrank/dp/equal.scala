package hackerrank.dp
import java.util.Scanner
import scala.annotation.tailrec

final case class Stats(chocolates: Array[Int], steps: Int, sum: Int)
final case class StepsAndRem(steps: Int, rem: Int, divisor: Int)
object Equal {

  type Chocolates = List[Int]
  implicit val chocs: Chocolates = List(5, 2, 1)

  def divMod[T: Integral](num: T, den: T): (T, T) = {
    val intInst = implicitly[Integral[T]]
    (intInst.quot(num, den), intInst.rem(num, den))
  }

  def minMod(num: Int)(den: Int): StepsAndRem = {
    val (steps, rem) = divMod(num, den)
    val mid          = den / 2
    if (rem <= mid) StepsAndRem(steps, rem, den)
    else StepsAndRem(steps + 1, den - rem, den)
  }

  private[this] def findOptimalStep(diff: Int)(implicit cs: Chocolates): StepsAndRem = {
    cs.map(minMod(diff)).filter(_.steps != 0).minBy(_.steps)
  }

  private def getMinIndex(a: Array[Int])(idx1: Int, idx2: Int): Int = {
    val elems        = Seq(idx1, idx2)
    val smallElemIdx = elems.indices.minBy(a)
    elems(smallElemIdx)
  }

  @tailrec
  private[this] def equalize(idx: Int)(stats: Stats): Stats = {
    val a     = stats.chocolates
    val start = a(idx - 1)
    val end   = a(idx)
    val diff  = end - start
    if (diff == 0) {
      stats
    } else {
      val nextSteps    = findOptimalStep(diff)
      val smallElemIdx = getMinIndex(a)(idx - 1, idx)
      val inc = nextSteps.steps * nextSteps.divisor
      a(smallElemIdx) += inc
      equalize(idx)(Stats(a, stats.steps + nextSteps.steps, stats.sum + inc))
    }
  }

  private[this] def incrementEnd(stats: Stats)(idx: Int): Stats = {
    val a = stats.chocolates
    a(idx) += stats.sum
    stats.copy(chocolates = a)
  }

  def getMinSteps(xs: IndexedSeq[Int]): Int = {
    val stats = Stats(xs.sorted.toArray, 0, 0)
    val results =
      (1 until xs.length).foldLeft(stats)((acc, idx) => equalize(idx)(incrementEnd(acc)(idx)))
    results.steps
  }

  private[this] def mainHelper(sc: Scanner, N: Int): IndexedSeq[Int] = {
    for {
      n <- 0 until N
    } yield sc.nextInt()
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in);
    val T  = sc.nextInt();
    for {
      t  <- 0 until T
      n  = sc.nextInt()
      xs = mainHelper(sc, n)
    } {
      val minSteps = getMinSteps(xs)
      println(s"${minSteps}")
    }
  }
}
