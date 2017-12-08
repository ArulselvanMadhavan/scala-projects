package clrs.chapter12.exercises

sealed trait BinarySearchTree[+T]

case class Node[T](value: T, left: BinarySearchTree[T], right: BinarySearchTree[T])
    extends BinarySearchTree[T]
case object Empty extends BinarySearchTree[Nothing]

object BinarySearchTree {

  def isValid[T: Ordering](t: BinarySearchTree[T]): Boolean = {
    val ordering = implicitly[Ordering[T]]
    t match {
      case Empty                 => true
      case Node(_, Empty, Empty) => true
      case Node(v, l @ Node(lval, _, _), Empty) =>
        ordering.lteq(lval, v) && isValid(l: BinarySearchTree[T])
      case Node(v, Empty, r @ Node(rval, _, _)) =>
        ordering.gt(rval, v) && isValid(r: BinarySearchTree[T])
      case Node(v, l @ Node(lval, _, _), r @ Node(rval, _, _)) =>
        ordering.lteq(lval, v) && ordering.gt(rval, v) && isValid(l: BinarySearchTree[T]) && isValid(
          r: BinarySearchTree[T])
    }
  }

  def insert[T: Ordering](t: BinarySearchTree[T], value: T): BinarySearchTree[T] = {
    val ordering = implicitly[Ordering[T]]
    t match {
      case Empty => Node(value, Empty, Empty)
      case Node(v, l, r) =>
        ordering.compare(value, v) match {
          case 1 => Node(v, l, insert(r, value))
          case _ => Node(v, insert(l, value), r)
        }
    }
  }

  def apply[T: Ordering](x: T*): BinarySearchTree[T] = {
    x.foldLeft(Empty: BinarySearchTree[T])(insert)
  }
}
