package clrs.chapter2.exercises
import scala.annotation.tailrec

object SumOfTwo {

  @tailrec
  private[this] def hasSum[T: Ordering: Numeric](xs: List[T], ys: List[T])(
      s: T): Boolean =
    (xs, ys) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case (x :: l1, y :: l2) => {
        implicitly[Ordering[T]]
          .compare(s, implicitly[Numeric[T]].plus(x, y)) match {
          case 0  => true
          case 1  => hasSum(l1, ys)(s)
          case -1 => hasSum(xs, l2)(s)
        }
      }
    }
  def checkSum[T: Ordering: Numeric](a: List[T])(x: T): Boolean = {
    val xs = MergeSortLists.sort(a)
    val ys = xs.reverse
    hasSum(xs, ys)(x)
  }
}
