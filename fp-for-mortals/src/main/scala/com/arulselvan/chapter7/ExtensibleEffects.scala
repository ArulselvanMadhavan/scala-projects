package com.arulselvan.chapter7

import scalaz._
import Scalaz._

object ExtensibleEffects {
  object MonadState {
    sealed abstract class Ast[S, A]
    final case class Get[S]()     extends Ast[S, S]
    final case class Put[S](s: S) extends Ast[S, Unit]

    def liftF[F[_], S](implicit I: Ast[S, ?] :<: F) =
      new MonadState[Free[F, ?], S] with BindRec[Free[F, ?]] {
        def get       = Free.liftF(I.inj(Get[S]()))
        def put(s: S) = Free.liftF(I.inj(Put[S](s)))

        val delegate                                                   = Free.freeMonad[F]
        def point[A](a: => A)                                          = delegate.point(a)
        def init                                                       = ???
        def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ???
        def tailrecM[A, B](f: A => Free[F, A \/ B])(a: A): Free[F, B]  = ???
      }
  }
}
