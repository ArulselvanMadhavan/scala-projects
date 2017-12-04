package clrs.chapter2.exercises

import scala.reflect.ClassTag
// import scala.annotation.tailrec

object MergeSort {

  private[this] def divide[T](a: Array[T]): (Array[T], Array[T]) =
    (a slice (0, a.length / 2), a slice (a.length / 2, a.length))

  private[this] def conquer[T: Ordering: ClassTag](a1: Array[T], a2: Array[T]): Array[T] = {
    val newArraySize = a1.length + a2.length
    val result = (0 until newArraySize).foldLeft((0, 0, new Array[T](newArraySize)))((acc, idx) => {
      if (acc._1 == a1.length) {
        //Pick second array
        acc._3(idx) = a2(acc._2)
        (acc._1, acc._2 + 1, acc._3)
      } else if (acc._2 == a2.length) {
        //Pick first array
        acc._3(idx) = a1(acc._1)
        (acc._1, acc._2 + 1, acc._3)
      } else {
        val lElem = a1(acc._1)
        val rElem = a2(acc._2)
        if (implicitly[Ordering[T]].lt(lElem, rElem)) {
          acc._3(idx) = lElem
          (acc._1 + 1, acc._2, acc._3)
        } else {
          acc._3(idx) = rElem
          (acc._1, acc._2 + 1, acc._3)
        }
      }
    })
    result._3
  }

  def sort[T: Ordering: ClassTag](a: Array[T]): Array[T] = {
    if (a.length == 1) a
    else {
      val parts = divide(a)
      conquer(sort(parts._1), sort(parts._2))
    }
  }

}
