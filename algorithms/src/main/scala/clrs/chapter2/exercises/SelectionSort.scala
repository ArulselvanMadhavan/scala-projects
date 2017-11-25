package clrs.chapter2.exercises

object SelectionSort {

  private[this] def updateMin[T: Ordering](a: Array[T], idx: Int): Array[T] = {
    val minIdx = (idx until a.length).foldRight(idx)(
      (currentIdx, minIdx) =>
        if (implicitly[Ordering[T]].lt(a(currentIdx), a(minIdx))) currentIdx
        else minIdx)
    val temp = a(minIdx)
    a(minIdx) = a(idx)
    a(idx) = temp
    a
  }

  def sort[T: Ordering](a: Array[T]): Array[T] =
    (0 until a.length - 1).foldLeft(a)(updateMin)
}
