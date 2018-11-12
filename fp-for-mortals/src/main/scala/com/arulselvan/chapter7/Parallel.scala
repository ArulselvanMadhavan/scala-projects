package com.arulselvan.chapter7
// import scalaz.{Applicative, Functor, Traverse, @@, Tags, Tag, IList}
// import scalaz.IList._
// import scalaz.Traverse._
import scalaz._
import Scalaz._
import scalaz.ioeffect._

object Parallel {

  object Applicative {
    // Tag Effect type F[_] with Parallel
    type Par[F[_]] = Applicative[λ[α => F[α] @@ Tags.Parallel]]
  }

  // implicit class TraverseSyntax[F[_], A](self: F[A]) {
  //   def parTraverse[G[_], B](f: A => G[B])(
  //       implicit F: Traverse[F],
  //       G: Applicative.Par[G]
  //   ): G[F[B]] = {
  //     type ParG[a] = G[a] @@ Tags.Parallel
  //     Tag.unwrap(F.traverse[ParG, A, B](self)(a => Tag(f(a))))
  //   }
  // }

  val input: IList[String]                    = ???
  def network(in: String): IO[Throwable, Int] = ???
  input.traverse(network): IO[Throwable, IList[Int]]
  // input.parTraverse(network): IO[Throwable, IList[Int]]
}
