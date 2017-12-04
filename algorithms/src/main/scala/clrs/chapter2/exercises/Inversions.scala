package clrs.chapter2.exercises

object Inversions {

  private[this] def combineHelper[T: Ordering](xs: List[T], ys: List[T]): (Int, List[T]) =
    (xs, ys) match {
      case (Nil, _) => (0, ys)
      case (_, Nil) => (0, xs)
      case (x :: l1, y :: l2) =>
        if (implicitly[Ordering[T]].lteq(x, y)) {
          val (count, res) = combineHelper(l1, ys)
          (count, x :: res)
        } else {
          val (count, res) = combineHelper(xs, l2)
          (count + xs.length, y :: res)
        }
    }

  private[this] def combine[T: Ordering](left: (Int, List[T]),
                                         right: (Int, List[T])): (Int, List[T]) = {
    val (lcount, xs): (Int, List[T]) = left
    val (rcount, ys)                 = right
    val (tcount, zs)                 = combineHelper(xs, ys)
    (lcount + rcount + tcount, zs)
  }

  private[this] def sort[T: Ordering](acc: (Int, List[T])): (Int, List[T]) = {
    val (k, xs) = acc
    xs match {
      case Nil      => acc
      case x :: Nil => acc
      case _ =>
        combine(sort((k, xs.slice(0, xs.length / 2))),
                sort((k, xs.slice(xs.length / 2, xs.length))))
    }
  }

  def count[T: Ordering](xs: List[T]): Int =
    sort((0, xs))._1
}
