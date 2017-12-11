package leetcode

object Jar extends Enumeration {
  type Jar = Value
  val Jar1, Jar2 = Value
}

import Jar._

sealed trait Action[+T] {
  def transferableCapacity(avail: Int, inTakeCapacity: Int): Int = {
    if (inTakeCapacity >= avail) avail
    else inTakeCapacity
  }
  def evaluate(mat: Array[Array[Boolean]])(current: (Int, Int)): Option[(Int, Int)]
}

case class EmptyJar[T](name: Jar) extends Action[T] {
  def evaluate(mat: Array[Array[Boolean]])(current: (Int, Int)): Option[(Int, Int)] = {
    name match {
      case Jar1 => if (mat(0)(current._2) == true) None else Some((0, current._2))
      case Jar2 => if (mat(current._1)(0) == true) None else Some((current._1, 0))
    }
  }
}

case class MoveJar[T](from: Jar, to: Jar) extends Action[T] {
  def evaluate(mat: Array[Array[Boolean]])(current: (Int, Int)): Option[(Int, Int)] = {
    val jar1Total = mat.size - 1
    val jar2Total = mat(0).size - 1
    from match {
      case Jar1 => {
        val transCap = transferableCapacity(current._1, jar2Total - current._2)
        val nextPos  = (current._1 - transCap, current._2 + transCap)
        if (mat(nextPos._1)(nextPos._2)) None
        else Some(nextPos)
      }
      case Jar2 =>
        val transCap = transferableCapacity(current._2, jar1Total - current._1)
        val nextPos  = (current._1 + transCap, current._2 - transCap)
        if (mat(nextPos._1)(nextPos._2)) None
        else Some(nextPos)
    }
  }
}

case class FillJar[T](name: Jar) extends Action[T] {
  def evaluate(mat: Array[Array[Boolean]])(current: (Int, Int)): Option[(Int, Int)] = {
    val rowSize = mat.size - 1
    val colSize = mat(0).size - 1
    name match {
      case Jar1 if (current._1 == 0) =>
        if (mat(rowSize)(current._2)) None else Some((rowSize, current._2))
      case Jar2 if (current._2 == 0) =>
        if (mat(current._1)(colSize)) None else Some((current._1, colSize))
      case _ => None
    }
  }
}

object WaterJugs {

  type Matrix2D = Array[Array[Boolean]]
  type Position = (Int, Int)

  def isMeasured(z: Int)(pos: Position): Boolean =
    pos._1 + pos._2 == z

  def nextStep(z: Int, mat: Matrix2D)(pos: Position,
                                      pathSoFar: List[Position]): Option[List[Position]] = {
    mat(pos._1)(pos._2) = true
    if (isMeasured(z)(pos)) Some(pathSoFar)
    else {
      val m1to2        = MoveJar(Jar1, Jar2).evaluate(mat)(pos)
      val m2to1        = MoveJar(Jar2, Jar1).evaluate(mat)(pos)
      val e1           = EmptyJar(Jar1).evaluate(mat)(pos)
      val e2           = EmptyJar(Jar2).evaluate(mat)(pos)
      val f1           = FillJar(Jar1).evaluate(mat)(pos)
      val f2           = FillJar(Jar2).evaluate(mat)(pos)
      val nextPosition = nextStep(z, mat)(_, _)
      (m1to2, m2to1, e1, e2, f1, f2) match {
        case (Some(nextPos), _, _, _, _, _) => nextPosition(nextPos, nextPos :: pathSoFar)
        case (_, Some(nextPos), _, _, _, _) => nextPosition(nextPos, nextPos :: pathSoFar)
        case (_, _, Some(nextPos), _, _, _) => nextPosition(nextPos, nextPos :: pathSoFar)
        case (_, _, _, Some(nextPos), _, _) => nextPosition(nextPos, nextPos :: pathSoFar)
        case (_, _, _, _, Some(nextPos), _) => nextPosition(nextPos, nextPos :: pathSoFar)
        case (_, _, _, _, _, Some(nextPos)) => nextPosition(nextPos, nextPos :: pathSoFar)
        case _                              => None
      }
    }
  }

  def isMeasurable(x: Int, y: Int, z: Int): Boolean = {
    if (z == 0) true
    else {
      val result = Array.fill(x + 1, y + 1)(false)
      result(0)(0) = true
      val paths = nextStep(z, result)((x, 0), List((x, 0)))
      paths.isEmpty == false
    }
  }
}
