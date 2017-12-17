package dp

object Stairs {

  def countSteps(a: Array[Int], step: Int): Array[Int] = {
    val oneStep  = if (step - 1 >= 0) a(step - 1) else 0
    val twoSteps = if (step - 2 >= 0) a(step - 2) else 0
    a(step) = oneStep + twoSteps
    a
  }

  def climbStairs(steps: Int): Int = {
    if (steps <= 1) 1
    else {
      val a = new Array[Int](steps + 1)
      a(0) = 1
      a(1) = 1
      val result = (2 to steps).foldLeft(a)(countSteps)
      result(steps)
    }
  }
}
