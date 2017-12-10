package leetcode

sealed trait ListWithPointers[+T]

final case class Node[+T](value: T, link: ListWithPointers[T], next: ListWithPointers[T])
    extends ListWithPointers[T]
final case object Empty extends ListWithPointers[Nothing]

object CopyListWithPointers {

  def apply[T](xs: T*) = {}

}
