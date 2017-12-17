package dp

object PaintHouse {

  def paintCost(costsSoFar: Array[Int])(cost: Int, currentColor: Int): Int = {
    costsSoFar.indices.filterNot(_ == currentColor).map(costsSoFar(_) + cost).min
  }

  def paintNext(costs: Array[Array[Int]])(acc: Array[Int], idx: Int): Array[Int] = {
    val currentCosts = costs(idx)
    val result       = (0 until currentCosts.length).map(x => paintCost(acc)(currentCosts(x), x)).toArray
    result
  }

  def minCost(costs: Array[Array[Int]]): Int = {
    if (costs.isEmpty) 0
    else
      (1 until costs.length).foldLeft(costs(0))(paintNext(costs)).min
  }
}
