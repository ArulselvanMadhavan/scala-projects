package clrs.chapter2.exercises

object BitAddition {

  private[this] def bitwiseAddition(xy: (Int, Int), acc: (Int, List[Int])): (Int, List[Int]) =
    (acc._1, xy._1, xy._2) match {
      case (1, 1, 1) => (1, 1 :: acc._2)
      case (0, 1, 1) => (1, 0 :: acc._2)
      case (1, 0, 1) => (1, 0 :: acc._2)
      case (1, 1, 0) => (1, 0 :: acc._2)
      case (_, _, _) => (0, (acc._1 + xy._1 + xy._2) :: acc._2)
    }

  def add(xs: List[Int], ys: List[Int]): List[Int] = {
    val result =
      (xs zip ys).foldRight((0, List()): (Int, List[Int]))(bitwiseAddition)
    result._1 :: result._2
  }
}
