object CollatzConjecture {

  private[this] def collatzSteps(n: Int, count: Int): Int =
    (n, n % 2) match {
      case (1, _) => count
      case (_, 0) => collatzSteps(n / 2, count + 1)
      case (_, _) => collatzSteps(3 * n + 1, count + 1)
    }

  def steps(n: Int): Option[Int] =
    if (n <= 0) None else Some(collatzSteps(n, 0))
}
