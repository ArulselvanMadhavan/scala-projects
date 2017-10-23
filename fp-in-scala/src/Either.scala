/**
  * Created by amadhavan1 on 7/23/17.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case _ => _
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case _ => _
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => _
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]):Either[E, List[A]] = {
    es.foldRight(Right(Nil):Either[E, List[A]])((x:Either[E, A], y:Either[E, List[A]]) => x.map2(y)(_ :: _))
  }

  def traverse[E, A, B](as:List[A])(f: A => Either[E, B]):Either[E, List[B]] = {
    as.foldRight(Right(Nil):Either[E, List[B]])((x:A, y:Either[E, List[B]]) => f(x).map2(y)(_ :: _))
  }
}
