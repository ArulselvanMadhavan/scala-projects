package utils

object FunctionUtils {
  def flip[A, B, C](f: A => B => C)(x: B)(y: A) = f(y)(x)
}
