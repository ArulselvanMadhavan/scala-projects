package com.arulselvan.chapter7
import scalaz._
import Scalaz._
import simulacrum._
import scala.annotation.tailrec

object AdvancedMonads {
  object Effects {
    final class IO[A](val interpret: () => A)
    object IO {
      def apply[A](a: => A): IO[A] = new IO(() => a)

      implicit val monad: Monad[IO] = new Monad[IO] {
        def point[A](a: => A): IO[A]                    = IO(a)
        def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(f(fa.interpret()).interpret())
      }
    }
  }

  object StackSafety {
    trait BindRec[F[_]] extends Bind[F] {
      def tailrecM[A, B](f: A => F[A \/ B])(a: A): F[B]
      override def forever[A, B](fa: F[A]): F[B] = ???
    }

    // The way to achieve stack safety is to convert method calls into references to an ADT, the Free Monad.
    sealed abstract class Free[S[_], A]
    object Free {
      private final case class Return[S[_], A](a: A)     extends Free[S, A]
      private final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]
      private final case class Gosub[S[_], A0, B](
          a: Free[S, A0],
          f: A0 => Free[S, B]
      ) extends Free[S, B] { type A = A0 }

      type Trampoline[A] = Free[() => ?, A]

      implicit val trampoline: Monad[Trampoline] with BindRec[Trampoline] =
        new Monad[Trampoline] with BindRec[Trampoline] {
          def point[A](a: => A): Trampoline[A]                                    = Return(a)
          def bind[A, B](fa: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = Gosub(fa, f)
          def tailrecM[A, B](f: A => Trampoline[A \/ B])(a: A): Trampoline[B] =
            bind(f(a)) {
              case -\/(a) => tailrecM(f)(a)
              case \/-(b) => point(b)
            }
        }

      object Trampoline {
        def done[A](a: A): Trampoline[A]                   = Return(a)
        def delay[A](a: => A): Trampoline[A]               = suspend(done(a))
        def suspend[A](a: => Trampoline[A]): Trampoline[A] = unit >> a
        private val unit: Trampoline[Unit]                 = Suspend(() => done(()))
        //         def run[A]: A = go(f => f())
        // def go[A](f: () => Trampoline[A] => Trampoline[A]):A = {
        //   @tailrec def go2(t:Trampoline[A]):A = t.resume match {
        //     case -\/(s) => go2(f(s))
        //     case \/-(r) => r
        //   }
        //   go2(this)
        // }
        // @tailrec def resume:() => Trampoline[A] \/ A = this match {
        //   case Return(a) => \/-(a)
        //   case Suspend(t) => -\/(t.map(Return(_)))
        //   case Gosub(Return(a), f) => f(a).resume
        //   case Gosub(Suspend(t), f) => -\/(t.map(f))
        //   case Gosub(Gosub(a, g), f) => a >>= (z => g(z) >>= f).resume
        // }
      }
    }
    // Free ADT is a natural data type representation of the Monad interface.
    // When an ADT mirrors the arguments of related functions, it is called Church Encoding.
  }

  object StackSafeDList {
    import StackSafety.Free._

    // final case class DList[A](f: IList[A] => Trampoline[IList[A]]) {
    //   def toIList: IList[A] = f(IList.empty).run
    //   def ++(as: =>DList[A]):DList[A] = DList(xs => suspend(as.f(xs) >>= f))
    // }
  }

  object MTL {
    @typeclass trait MonadTrans[T[_[_], _]] {
      def liftM[F[_]: Monad, A](a: F[A]): T[F, A]
    }

    @typeclass trait Hoist[F[_[_], _]] extends MonadTrans[F] {
      def hoist[M[_]: Monad, N[_]](f: M ~> N): F[M, ?] ~> F[N, ?]
    }

    final case class MaybeT[F[_], A](run: F[Maybe[A]]) { self =>
      import Maybe._

      def orElse(a: => MaybeT[F, A])(implicit F: Monad[F]): MaybeT[F, A] =
        MaybeT(F.bind(run)(_.cata(a => F.point(just(a)), a.run)))
    }

    object MaybeT {

      def just[F[_]: Applicative, A](v: => A): MaybeT[F, A] =
        MaybeT(Maybe.just(v).pure[F])

      def empty[F[_]: Applicative, A]: MaybeT[F, A] =
        MaybeT(Maybe.empty.pure[F])
    }

    implicit def monad[F[_]: Monad] = new MonadPlus[MaybeT[F, ?]] {
      def point[A](a: => A): MaybeT[F, A] = MaybeT.just(a)
      def bind[A, B](fa: MaybeT[F, A])(f: A => MaybeT[F, B]): MaybeT[F, B] =
        MaybeT(fa.run >>= (_.cata(f(_).run, Maybe.empty.pure[F])))
      def empty[A]: MaybeT[F, A]                                     = MaybeT.empty
      def plus[A](a: MaybeT[F, A], b: => MaybeT[F, A]): MaybeT[F, A] = a orElse b
    }

    final case class EitherT[F[_], A, B](run: F[A \/ B])
    object EitherT {
      def either[F[_]: Applicative, A, B](d: A \/ B): EitherT[F, A, B] = ???
      def leftT[F[_]: Functor, A, B](fa: F[A]): EitherT[F, A, B]       = ???
      def rightT[F[_]: Functor, A, B](db: F[B]): EitherT[F, A, B]      = ???
      def pureLeft[F[_]: Applicative, A, B](a: A): EitherT[F, A, B]    = ???
      def pure[F[_]: Applicative, A, B](b: B): EitherT[F, A, B]        = ???
    }

    @typeclass trait MonadError[F[_], E] extends Monad[F] {
      def raiseError[A](e: E): F[A]
      def handleError[A](fa: F[A])(f: E => F[A]): F[A]
    }

    implicit final class MonadErrorOps[F[_], E, A](self: F[A])(implicit val F:MonadError[F, E]){
      def attempt:F[E \/ A]
      def recover(f: E => A): F[A]
      def emap[B](f: A => E \/ B): F[B]
    }
  }
}
