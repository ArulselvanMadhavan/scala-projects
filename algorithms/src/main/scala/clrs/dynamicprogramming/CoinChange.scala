package clrs.dynamicProgramming

//1. We are going to have a finite set of denominations.
//2. The combination of denominations doesn't allow for a greedy choice.
//3. Task: Find the minimum number of coins that are needed to denote a value of V.
//4. Coins Array - C -(1,2,5,10,20,50,100) - Sorted
//5. Denomination Int - V
object CoinChange {

  private[this] def safeMatrixAccess(row: Int, col: Int)(mat: Array[Array[Int]],
                                                         r: Int,
                                                         c: Int): Option[Int] = {
    ((r >= 0 && r < row), (c >= 0 && c < col)) match {
      case (true, true) => Some(mat(r)(c))
      case _            => None
    }
  }

  def findMinCoins(coins: Array[Int])(value: Int): Unit = {
    val row     = value + 1
    val col     = coins.length
    val res     = Array.fill(row, col)(0)
    val safeGet = safeMatrixAccess(row, col)(_, _, _);
    for {
      v    <- (1 to value)
      cidx <- (0 until coins.length)
      c    = coins(cidx)
    } {
      val prevCost   = safeGet(res, v, cidx - 1).getOrElse(Int.MaxValue)
      val newCost    = safeGet(res, v - c, cidx).getOrElse(Int.MaxValue - 1) + 1
      val costs      = Seq(prevCost, newCost)
      val argMinCost = costs.indices.minBy(costs)
      res(v)(cidx) = costs(argMinCost)
    }
    for {
      r <- 0 until row
      c <- 0 until col
    } print(s"${(r, c)}\t${res(r)(c)}\n")
  }
}
