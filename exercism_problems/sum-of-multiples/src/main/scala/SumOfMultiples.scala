object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = 
    (1 until limit).toList.filter((x:Int) => factors.filter((y:Int) => (x % y == 0)).isEmpty == false).sum
}

