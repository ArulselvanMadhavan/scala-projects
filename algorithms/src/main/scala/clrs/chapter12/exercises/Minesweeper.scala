package clrs.chapter12.exercises

import scala.util.Random;

sealed trait Cell

final case class EmptyCell() extends Cell
final case class Bomb() extends Cell
final case class Value(x:Int) extends Cell

object Minesweeper {
  type Board = Array[Array[Cell]]
  
  def generateRandomPosition(size:Int):(Int, Int) = {
    val rnd = new Random();
    val x = rnd.nextInt(size)
    val y = rnd.nextInt(size)
    (x, y)
  }

  //If the (x,y) is a Bomb location ->
  //If (x,y) is an emptyCell -> Board with some perimeter around (x,y) revealed with Either a Value or Empty Cell
  def acceptClick(x:Int, y:Int, a:Board):Option[Board] = {
    a(x)(y) match {
      case Bomb() => None
        ///TODO
              // case EmptyCell =>
        // 1. Find the closest Bomb Cell location in the same row, next row and previous row.
          // 2. Compute 
    }
  }

  def placeBombs(a:Board, numOfBombs:Int):Board = {
    var n = numOfBombs
    while(n > 0) {
      val (x, y) = generateRandomPosition(a.length)
      a(x)(y) match {
        case EmptyCell() => {
          n = n - 1
          a(x)(y) = Bomb()
        }
        case _ => n = n
      }
    }
    a
  }

  def printRow(r:Array[Cell]):String = {
    r.mkString("\t")
  }

  def printBombs(a:Board):Unit = {
    for {
      i <- 0 until a.length
    } println(printRow(a(i)))
  }

  def main(args: Array[String]):Unit = {
    val rows = 5
    val columns = 5
    val board:Array[Array[Cell]] = Array.fill[Cell](5,5)(EmptyCell());
    val boardWithBombs = placeBombs(board, 10)
    println(s"${printBombs(boardWithBombs)}")
  }
}
