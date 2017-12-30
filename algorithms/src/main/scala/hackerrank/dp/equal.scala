package hackerrank.dp
import java.util.Scanner
import scala.annotation.tailrec

final case class Stats(chocolates: Array[Int], steps: Int, sum: Int)
final case class StepsAndRem(steps: Int, rem: Int, divisor: Int) extends Ordered[StepsAndRem] {
  def compare(that: StepsAndRem): Int = {
    (this.steps compare that.steps) match {
      case 0 => this.rem compare that.rem
      case c => c
    }
  }
}

object Equal {

  type Chocolates = List[Int]
  implicit val chocs: Chocolates = List(1, 2, 5)

  def divMod[T: Integral](num: T, den: T): (T, T) = {
    val intInst = implicitly[Integral[T]]
    (intInst.quot(num, den), intInst.rem(num, den))
  }

  def minMod(idx: Int, num: Int)(den: Int): StepsAndRem = {
    val (steps, rem) = divMod(num, den)
    if (idx == 1) {
      val mid = den / 2
      if (rem <= mid) StepsAndRem(steps, rem, den)
      else StepsAndRem(steps + 1, den - rem, den)
    } else StepsAndRem(steps, rem, den)
  }

  private[this] def findOptimalStep(idx: Int, diff: Int)(implicit cs: Chocolates): StepsAndRem = {
    cs.map(minMod(idx, diff)).filter(_.steps != 0).min
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
    val diff  = (end - start).abs
    if (diff == 0) {
      stats
    } else {
      val nextSteps    = findOptimalStep(idx, diff)
      val smallElemIdx = getMinIndex(a)(idx - 1, idx)
      val inc          = nextSteps.steps * nextSteps.divisor
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
