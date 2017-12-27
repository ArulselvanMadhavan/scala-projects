package hackerrank.warmup

object Solution {

  type Scores = (Int, Int)

  private[this] def helper[T: Ordering](acc: Scores, scores: (T, T)): Scores = {
    val ordInst = implicitly[Ordering[T]]
    ordInst.compare(scores._1, scores._2) match {
      case 1  => (acc._1 + 1, acc._2)
      case 0  => acc
      case -1 => (acc._1, acc._2 + 1)
    }
  }

  private[this] def computeScores[T: Ordering](l1: Seq[T], l2: Seq[T]): Scores = {
    (l1 zip l2).foldLeft((0, 0))(helper)
  }

  def main(args: Array[String]): Unit = {
    val sc     = new java.util.Scanner(System.in);
    val a0     = sc.nextInt();
    val a1     = sc.nextInt();
    val a2     = sc.nextInt();
    val b0     = sc.nextInt();
    val b1     = sc.nextInt();
    val b2     = sc.nextInt();
    val alice  = Seq(a0, a1, a2)
    val bob    = Seq(b0, b1, b2)
    val scores = computeScores(alice, bob)
    println(s"${scores._1} ${scores._2}")
  }
}
