package utils

object TwoDimArray {

  type TwoDMatrix[T] = Array[Array[T]]

  def printMatrix[T](a: TwoDMatrix[T]): Unit = {
    for {
      i <- 0 until a.length
    } println(a(i).mkString("\t"))
  }

  def findMax[T: Ordering](a: TwoDMatrix[T]): T = {
    val result = for {
      i <- 0 until a.length
    } yield a(i).max
    result.max
  }

}
