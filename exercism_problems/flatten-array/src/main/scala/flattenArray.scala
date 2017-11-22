object FlattenArray {
  def flatten(xs:List[Any]):List[Any] =
    xs match {
      case Nil => Nil
      case (x @ (_ :: _)) :: ys => flatten(x) ++ flatten(ys)
      case null :: ys => flatten(ys)
      case y :: ys => y :: flatten(ys)
    }
}
