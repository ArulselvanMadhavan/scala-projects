package leetcode

object SpiralMatrix {

  type PrintCounter = (Int, Int, Int, Int) //(Right,Down,Left,Up)
  type MatrixDim    = (Int, Int)

  def getMatrixDim(a: Array[Array[Int]]): MatrixDim = {
    (a.size, a(0).size)
  }

  def moveRight(a: Array[Array[Int]])(
                                      c: PrintCounter): PrintCounter = {
    val i      = 0 + c._1
    val (m, n) = getMatrixDim(a)
    for {
      j <- 0 + c._4 until n - c._2
    } println(s"${i}\t${j}\t${a(i)(j)}")
    (c._1 + 1, c._2, c._3, c._4)
  }

  def moveDown(a: Array[Array[Int]])( c: PrintCounter): PrintCounter = {
    val (m, n) = getMatrixDim(a)
    val j      = n - c._2 - 1
    for {
      i <- 0 + c._1 until m - c._3
    } println(s"${i}\t${j}\t${a(i)(j)}")
    (c._1, c._2 + 1, c._3, c._4)
  }

  def moveLeft(a: Array[Array[Int]])( c: PrintCounter):  PrintCounter = {
    val (m, n) = getMatrixDim(a)
    val i      = m - c._3 - 1
    for {
      j <- n - c._2 - 1 to 0 + c._4 by -1
    } println(s"${i}\t${j}\t${a(i)(j)}")
    (c._1, c._2, c._3 + 1, c._4)
  }

  def moveUp(a:Array[Array[Int]])(c:PrintCounter):PrintCounter = {
    val (m, n) = getMatrixDim(a)
    val j = 0 + c._4
    for {
      i <- m - 1 - c._3 to 0 + c._1 by -1
    } println(s"${i}\t${j}\t${a(i)(j)}")
(c._1,c._2,c._3,c._4+1)
  }

  def spiralPrint(a: Array[Array[Int]]): Unit = {
    val (m, n) = getMatrixDim(a)
    var c      = (0, 0, 0, 0)
    while (c._1 + c._3 < m && c._2 + c._4 < n) {
      val result  = moveRight(a)(c)
      val result2 = moveDown(a)(result)
      val result3 = moveLeft(a)(result2)
      c = moveUp(a)(result3)
    }
  }
}
