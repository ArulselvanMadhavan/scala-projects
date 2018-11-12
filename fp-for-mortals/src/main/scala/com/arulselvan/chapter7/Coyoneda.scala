package com.arulselvan.chapter7
import scalaz._

object Coyoneda {
  sealed abstract class Coyoneda[S[_], A] {
    def run(implicit S: Functor[S]): S[A]      = ???
    def trans[G[_]](f: S ~> G): Coyoneda[G, A] = ???
  }

  object Coyoneda {
    implicit def functor[S[_], A]: Functor[Coyoneda[S, ?]] = ???
    final case class Map[F[_], A, B](fa: F[A], f: A => B) extends Coyoneda[F, A]
    def apply[S[_], A, B](sa: S[A])(f: A => B) = Map[S, A, B](sa, f)
    def lift[S[_], A](sa: S[A])                = Map[S, A, A](sa, identity)
  }

  sealed abstract class ContravariantCoyoneda[S[_], A] {
    def run(implicit S: Contravariant[S]): S[A]             = ???
    def trans[G[_]](f: S ~> G): ContravariantCoyoneda[G, A] = ???
  }

  object ContravariantCoyoneda {
    implicit def contravariant[S[_], A]: Contravariant[ContravariantCoyoneda[S, ?]] = ???

    final case class ContraMap[F[_], A, B](fa: F[A], f: B => A) extends ContravariantCoyoneda[F, A]
    def apply[F[_], A, B](sa: F[A])(f: B => A) = ContraMap(sa, f)
    def lift[S[_], A](sa: S[A]): ContravariantCoyoneda[S, A] =
      ContraMap[S, A, A](sa, identity)
  }
}
