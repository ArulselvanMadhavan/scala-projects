package hackerrank.warmup
import java.util.Scanner

object BigSum {

  def computeSum(xs:IndexedSeq[Int]):Long = xs.foldLeft(0L)(_ + _)

  def main(args:Array[String]):Unit = {
    val sc = new Scanner(System.in)
    val results = for {
      t <- 0 until sc.nextInt()
    } yield sc.nextInt()
    println(computeSum(results))
  }
}
