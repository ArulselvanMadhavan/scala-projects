package clrs.chapter2.exercises

object MergeSortLists {

  private[this] def divide[T](ys: List[T]): (List[T], List[T]) =
    (ys slice (0, ys.length / 2), ys slice (ys.length / 2, ys.length))

  private[this] def conquer[T: Ordering](xs: List[T], ys: List[T]): List[T] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: l1, y :: l2) =>
        if (implicitly[Ordering[T]] lt (x, y)) x :: conquer(l1, ys)
        else y :: conquer(xs, l2)
    }

  def sort[T: Ordering](xs: List[T]): List[T] =
    xs match {
      case x :: Nil => xs
      case _ => {
        val parts = divide(xs)
        conquer(sort(parts._1), sort(parts._2))
      }
    }
}
