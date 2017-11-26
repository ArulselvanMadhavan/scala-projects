package clrs.insertionSort

object InsertionSort {

  def switchElements[T: Ordering](currentIndex: Int, a: Array[T]): Array[T] = {
    val nextIndex = currentIndex + 1
    if (implicitly[Ordering[T]] lt (a(nextIndex), a(currentIndex))) {
      val currentElem = a(currentIndex)
      a(currentIndex) = a(nextIndex)
      a(nextIndex) = currentElem
    }
    a
  }

  def placeElement[T: Ordering](a: Array[T], end: Int): Array[T] =
    (0 until end).foldRight(a)(switchElements)

  def sort[T: Ordering](a: Array[T]): Array[T] = {
    (1 until a.length).foldLeft(a)(placeElement)
  }
}
