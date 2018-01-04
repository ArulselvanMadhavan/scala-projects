package hackerrank.sample
import java.util.Scanner;

object FindNumber {

  def findNumber(arr: Array[Int], k: Int): String = {
    arr.find(_ == k) match {
      case Some(_) => "YES"
      case None    => "NO"
    }
  }

  def findOddNumbers(l: Int, r: Int): Array[Int] = {
    (l to r).filter(_ % 2 == 1).toArray
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val T  = sc.nextInt();
    // val n  = sc.nextInt()
    val results = for {
      t <- 0 until T
    } yield sc.nextInt()
    println(s"${results}")
  }

}
