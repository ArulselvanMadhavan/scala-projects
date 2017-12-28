package hackerrank.dp
import java.util.Scanner
import scala.annotation.tailrec

object Equal {

  type Chocolates = List[Int]
  type Stats      = (List[Int], Int)
  implicit val chocs: Chocolates = List(5, 2, 1)

  def divMod[T: Integral](num: T, den: T): (T, T) = {
    val intInst = implicitly[Integral[T]]
    (intInst.quot(num, den), intInst.rem(num, den))
  }

  @tailrec
  private[this] def countMin(acc: Int, diff: Int)(implicit cs: Chocolates): Int = {
    if (diff == 0) acc
    else if (cs.head <= diff) {
      val (quot, rem) = divMod(diff, cs.head)
      countMin(acc + quot, rem)(cs.tail)
    } else countMin(acc, diff)(cs.tail)
  }

  private[this] def wrapper(xs: IndexedSeq[Int])(acc: Stats, idx: Int): Stats = {
    val start = xs(idx - 1) + acc._1.tail.sum
    val end   = xs(idx) + acc._1.sum
    val diff  = end - start
    val steps = countMin(acc._2, diff)
    (diff :: acc._1, steps)
  }

  def getMinSteps(xs: IndexedSeq[Int]): Int = {
    val xs_sorted = xs.sorted
    val results   = (1 until xs.length).foldLeft((0 :: Nil: List[Int], 0))(wrapper(xs_sorted))
    results._2
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
