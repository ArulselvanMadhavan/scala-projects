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
      implicit def monadPlus[F[_]: Monad] = new MonadPlus[MaybeT[F, ?]] {
        def point[A](a: => A): MaybeT[F, A] = MaybeT.just(a)
        def bind[A, B](fa: MaybeT[F, A])(f: A => MaybeT[F, B]): MaybeT[F, B] =
          MaybeT(fa.run >>= (_.cata(f(_).run, Maybe.empty.pure[F])))
        def empty[A]: MaybeT[F, A]                                     = MaybeT.empty
        def plus[A](a: MaybeT[F, A], b: => MaybeT[F, A]): MaybeT[F, A] = a orElse b
      }

    }
    final case class EitherT[F[_], A, B](run: F[A \/ B])
    object EitherT {
      def either[F[_]: Applicative, A, B](d: A \/ B): EitherT[F, A, B] = ???
      def leftT[F[_]: Functor, A, B](fa: F[A]): EitherT[F, A, B]       = ???
      def rightT[F[_]: Functor, A, B](db: F[B]): EitherT[F, A, B]      = ???
      def pureLeft[F[_]: Applicative, A, B](a: A): EitherT[F, A, B]    = ???
      def pure[F[_]: Applicative, A, B](b: B): EitherT[F, A, B]        = ???
    }

    trait MonadError[F[_], E] extends Monad[F] {
      def raiseError[A](e: E): F[A]                    //throw
      def handleError[A](fa: F[A])(f: E => F[A]): F[A] //catch
    }

    implicit def monadError[F[_]: Monad, E] = new MonadError[EitherT[F, E, ?], E] {
      def bind[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] =
        EitherT(fa.run >>= (_.fold(_.left[B].pure[F], b => f(b).run)))
      def point[A](a: => A): EitherT[F, E, A]   = EitherT.pure(a)
      def raiseError[A](e: E): EitherT[F, E, A] = EitherT.pureLeft(e)
      def handleError[A](fa: EitherT[F, E, A])(f: E => EitherT[F, E, A]): EitherT[F, E, A] =
        EitherT(fa.run >>= {
          case -\/(e) => f(e).run
          case right  => right.pure[F]
        })
    }

    implicit final class MonadErrorOps[F[_], E, A](self: F[A])(implicit val F: MonadError[F, E]) {
      def attempt: F[E \/ A]            = ??? // Brings errors into values.
      def recover(f: E => A): F[A]      = ??? // Turning an error into a value for all cases.
      def emap[B](f: A => E \/ B): F[B] = ??? // Apply transformations that can fail.
    }

    type ReaderT[F[_], A, B] = Kleisli[F, A, B]

    final case class Kleisli[F[_], A, B](run: A => F[B]) {
      def dimap[C, D](f: C => A, g: B => D)(implicit F: Functor[F]): Kleisli[F, C, D] = ???
      def >=>[C](k: Kleisli[F, B, C])(implicit F: Bind[F]): Kleisli[F, A, C]          = ???
      def >==>[C](k: B => F[C])(implicit F: Bind[F]): Kleisli[F, A, C]                = this >=> Kleisli(k)
    }

    object Kleisli {
      implicit def kleisliFn[F[_], A, B](k: Kleisli[F, A, B]): A => F[B] = k.run
    }

    // trait ConfigReader[F[_]] {
    //   def token: F[RefreshToken]
    // }

    trait MonadReader[F[_], S] extends Monad[F] {
      def ask: F[S]
      def local[A](f: S => S)(fa: F[A]): F[A]
    }

    implicit def monadReader[F[_]: Monad, R] = new MonadReader[Kleisli[F, R, ?], R] {
      def point[A](a: => A): Kleisli[F, R, A] = Kleisli(_ => Applicative[F].point(a))
      def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]) =
        Kleisli(r => Monad[F].bind(fa.run(r))(a => f(a).run(r)))
      def ask: Kleisli[F, R, R] = Kleisli(_.pure[F])
      def local[A](f: R => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] =
        Kleisli(f andThen fa.run)
    }

    final case class WriterT[F[_], W, A](run: F[(W, A)])
    object WriterT {

      def put[F[_]: Functor, W, A](value: F[A])(w: W): WriterT[F, W, A] = ???
      def putWith[F[_]: Functor, W, A](value: F[A])(w: A => W): WriterT[F, W, A] =
        WriterT(value.map(a => (w(a), a)))

      trait MonadTell[F[_], W] extends Monad[F] {
        def writer[A](w: W, v: A): F[A]
        def tell(w: W): F[Unit]
        def :++>[A](fa: F[A])(w: => W): F[A]
        def :++>>[A](fa: F[A])(f: A => W): F[A]
      }

      trait MonadListen[F[_], W] extends MonadTell[F, W] {
        def listen[A](fa: F[A]): F[(A, W)]
        def written[A](fa: F[A]): F[W]
      }

      // implicit def monadListen[F[_]: Monad, W: Monoid] = new MonadListen[WriterT[F, W, ?], W] {
        // def point[A](a: => A) = WriterT((Monoid[W].zero, a).point)
        // def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) = WriterT(
          // fa.run >>= { case (wa, a) => f(a).run.map { case (wb, b) => (wa |+| wb, b)}}
        // )
        // def writer[A](w: W, v: A) = WriterT((w -> v).point)
        // def listen[A](fa:WriterT[F,W,A]) = WriterT(
          // fa.run.map {case (w, a) => (w, (a, w))}
        // )
        // }
      // sealed trait Log
      // final case class Debug(msg: String)(implicit m: Meta) extends Log
      // final case class Info(msg: String)(implicit m: Meta) extends Log
      // final case class Warning(msg: String)(implicit m: Meta) extends Log
    }

    object StateThings {

      trait MonadState[F[_], S] extends Monad[F] {
        def put(s: S): F[Unit]
        def get: F[S]
        def modify(f: S => S): F[Unit] = get >>= (s => put(f(s)))
      }

      sealed abstract class StateT[F[_], S, A] {
        import StateT._
        def run(initial: S)(implicit F: Monad[F]): F[(S, A)] = this match {
          case Point(f) => f(initial)
          case FlatMap(Point(f), g) =>
            f(initial) >>= {case (s,a) => g(s, a).run(s)}
          case FlatMap(FlatMap(f,g), h) =>
            FlatMap(f, (s, x) => FlatMap(g(s, x), h)).run(initial)
        }
      }

      object StateT {
        def apply[F[_], S, A](f: S => F[(S, A)]): StateT[F, S, A] = Point(f)
        private final case class Point[F[_], S, A](
          run: S => F[(S, A)]
        ) extends StateT[F, S, A]
        private final case class FlatMap[F[_], S, A, B](
          a: StateT[F, S, A],
          f: (S, A) => StateT[F, S, B]
        ) extends StateT[F, S, B]
        def stateT[F[_]: Applicative, S, A](a: A): StateT[F, S, A] = ???
      }

      implicit def monad[F[_] : Applicative, S] = new MonadState[StateT[F, S, ?], S] {
        def point[A](a: =>A):StateT[F, S, A] = Point(s => (s, a).point[F])
        def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]):StateT[F, S, B] =
          FlatMap(fa, (_, a:A) => f(a))
        def get = Point(s => (s, s).point[F])
        def put(s: S) = Point(_ => (s, ()).point[F])
      }
    }
  }
}
