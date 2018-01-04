package hackerrank.warmup
import java.util.Scanner

object BigSum {

  def computeSum(xs:IndexedSeq[Long]):Long = xs.sum

  def main(args:Array[String]):Unit = {
    val sc = new Scanner(System.in)
    val results = for {
      t <- 0 to sc.nextInt()
    } yield sc.nextLong()
    computeSum(results)
  }
}
