package utils

object MathUtils {
  def largestNthRoot(x: Double, pow: Double): Int = {
    math.pow(x, pow).toInt
  }
}
