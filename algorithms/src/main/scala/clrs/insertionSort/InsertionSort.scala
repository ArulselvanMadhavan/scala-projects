package clrs.insertionSort
import scala.annotation.tailrec

object InsertionSort {

  // @tailrec
  // private[this] def findMinIndex[T: Ordering](elem: T,
  //                                             start: Int,
  //                                             end: Int,
  //                                             a: Array[T]): Int = {
  //   if (start == end) end
  //   else if (implicitly[Ordering[T]].lt(elem, a(start))) start
  //   else findMinIndex(elem, start + 1, end, a)
  // }

  // private[this] def swap[T](src: Int, dest: Int, a: Array[T]): Array[T] = {
  //   val temp = a(src)
  //   a(src) = a(dest)
  //   a(dest) = temp
  //   a
  // }

  // private[this] def shiftByOne[T](start:Int, a:Array[T]):Array[T] =
  //   for (start until a.length) {
  //     val temp = a(start)
  //   }

  private[this] def shiftByOne[T](x: T,
                                  start: Int,
                                  end: Int,
                                  a: Array[T]): Array[T] =
    if (start == end) {
      a(end) = x
      a
    } else {
      val next = a(start)
      a(start) = x
      shiftByOne(next, start + 1, end, a)
    }

  def sort[T: Ordering](b: Array[T]): Array[T] = {
    var a = b
    for (i <- 1 until a.length) {
      var j = 0
      while (j < i) {
        if (implicitly[Ordering[T]] lt (a(i), a(j))) {
          a = shiftByOne(a(i), j, i, a)
          j = i
        }
        j += 1
      }
    }
    a
  }
}
