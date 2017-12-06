package clrs.chapter6.exercises

sealed trait BinaryHeap[+T]

case class Node[T](value: T, left: BinaryHeap[T], right: BinaryHeap[T]) extends BinaryHeap[T]
case object Empty                                                       extends BinaryHeap[Nothing]

object BinaryHeap {

  private[this] def max(x: Int, y: Int): Int =
    if (x < y) y else x

  def height[T](t: BinaryHeap[T]): Int = {
    t match {
      case Empty                => 0
      case Node(_, left, right) => max(height(left), height(right)) + 1
    }
  }

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
