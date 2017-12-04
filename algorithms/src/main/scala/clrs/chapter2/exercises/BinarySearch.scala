package clrs.chapter2.exercises

object BinarySearch {

  def search[T: Ordering](a: Array[T])(x: T): Option[Int] = {
    if (a.length == 1) if (a(0) == x) Some(0) else None
    else {
      implicitly[Ordering[T]].compare(x, a(a.length / 2)) match {
        case 0  => Some(a.length / 2)
        case -1 => search(a slice (0, a.length / 2))(x)
        case 1 =>
          search(a slice (a.length / 2, a.length))(x).map(idx => idx + a.length / 2)
      }
    }
  }

}
