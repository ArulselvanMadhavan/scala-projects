class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] =
    for(y <- list) yield f(y)
}
