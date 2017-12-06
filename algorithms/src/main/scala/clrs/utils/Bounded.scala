package clrs.utils

sealed trait Bounded[A] {
  def minValue: A
  def maxValue: A
}

object Bounded {
  def apply[A](min: A, max: A) = new Bounded[A] {
    def minValue = min
    def maxValue = max
  }
  implicit val intBounded   = Bounded(Int.MinValue, Int.MaxValue)
  implicit val floatBounded = Bounded(Float.MinValue, Float.MaxValue)
  implicit val loangBounded = Bounded(Long.MinValue, Long.MaxValue)
}
