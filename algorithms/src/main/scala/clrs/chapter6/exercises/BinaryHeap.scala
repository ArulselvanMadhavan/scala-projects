package clrs.chapter6.exercises
import clrs.utils.MathUtils

sealed trait BinaryHeap[+T]

case class Node[T](value: T, left: BinaryHeap[T], right: BinaryHeap[T]) extends BinaryHeap[T]
case object Empty                                                       extends BinaryHeap[Nothing]

object BinaryHeap {

  private[this] def swap[T](a: Array[T])(src: Int, dest: Int): Array[T] = {
    val tmp = a(dest)
    a(dest) = a(src)
    a(src) = tmp
    a
  }

  private[this] def adjustHeap[T: Ordering](
      a: Array[T])(idx: Int, lIdx: Int, rIdx: Int): Array[T] = {
    val maxIdx = Seq[Int](idx, lIdx, rIdx).maxBy(a)
    maxIdx match {
      case `idx`  => a
      case `lIdx` => maxHeapify(swap(a)(idx, lIdx), lIdx)
      case `rIdx` => maxHeapify(swap(a)(idx, rIdx), rIdx)
    }
  }

  private[this] def maxHeapify[T: Ordering](a: Array[T], idx: Int): Array[T] = {
    (lchild(idx) < a.length, rchild(idx) < a.length) match {
      case (true, true) => adjustHeap(a)(idx, lchild(idx), rchild(idx))
      case (true, false) =>
        if (implicitly[Ordering[T]].lt(a(idx), a(lchild(idx)))) swap(a)(idx, lchild(idx)) else a
      case (_, _) => a
    }
  }

  def heapify[T: Ordering](a: Array[T]): Array[T] = {
    val h = height(a)
    (h to 0 by -1).foldLeft(a)(maxHeapify)
  }

  def height[T](a: Seq[T]): Int = math.floor(MathUtils.log2(a.length.toDouble)).toInt

  private[this] def lchild(idx: Int): Int = 2 * idx + 1
  private[this] def rchild(idx: Int): Int = 2 * idx + 2

  private[this] def buildTree[T](a: Seq[T])(idx: Int): BinaryHeap[T] = {
    ((lchild(idx) < a.length), (rchild(idx) < a.length)) match {
      case (false, _)    => Node(a(idx), Empty, Empty)
      case (true, false) => Node(a(idx), buildTree(a)(lchild(idx)), Empty)
      case (_, true)     => Node(a(idx), buildTree(a)(lchild(idx)), buildTree(a)(rchild(idx)))
    }
  }

  def apply[T](xs: T*): BinaryHeap[T] = {
    if (xs.isEmpty) Empty
    else buildTree(xs)(0)
  }
}
