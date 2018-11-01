package com.arulselvan.chapter5
import scalaz._
import simulacrum._
import Scalaz._

//Divisible -> Divide -> Contravariant -> InvariantFunctor
//Comonad -> Cobind -> Functor -> InvariantFunctor
//Bind -> Apply -> Functor
//Align -> Functor
//Applicative -> Apply
//Monad -> Applicative
//MonadPlus -> Monad
//Advanced Monads -> Monad -> Applicative
//MonadPlus -> ApplicativePlus -> Applicative
//Traverse1 -> Traverse -> Functor
//Traverse -> Foldable
//Traverse1 -> Foldable1 -> Foldable
//ApplicativePlus -> PlusEmpty -> Plus
//IsEmpty -> PlusEmpty -> Plus

//Monoid -> Semigroup
//Band -> Semigroup
//Enum -> Order -> Equal
//Bitraverse -> Bifunctor
//Bitraverse -> Bifoldable

//zip, unzip, cozip, show, optional

object Typeclasses {

  // For combining two values.
  // Should be associative
  @typeclass trait Semigroup[A] {
    @op("|+|") def append(x: A, y: => A): A
    def multiply1(value: A, n: Int): A
  }

  // Monoid - Semigroup with zero(empty or identity)
  @typeclass trait Monoid[A] {
    def zero: A
    def multiply(value: A, n: Int): A =
      if (n <= 0) zero else multiply(value, n - 1)
  }

  // Band - Append operation of two same elements is idempotent.(gives the same value)
  @typeclass trait Band[A] extends Semigroup[A]

  sealed abstract class Currency
  case object EUR extends Currency
  case object USD extends Currency
  case object INR extends Currency

  final case class TradeTemplate(
      payments: List[java.time.LocalDate],
      ccy: Option[Currency],
      otc: Option[Boolean]
  )

  object TradeTemplate {
    import Monoid._
    // implicit val monoid: Monoid[TradeTemplate] = Monoid.instance((a, b) => TradeTemplate(
    //   a.payments |+| b.payments,
    //   a.ccy |+| b.ccy,
    //   a.otc |+| b.otc
    // ),
    //   TradeTemplate(Nil, None, None)
    // )
  }

  @typeclass trait Equal[F] {
    @op("===") def equal(a1: F, a2: F): Boolean
    @op("/==") def notEqual(a1: F, a2: F): Boolean = !equal(a1, a2)
  }

  object Mappable {
    @typeclass trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
      def void[A](fa: F[A]): F[Unit]                     = map(fa)(_ => ())
      def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => (a, f(a)))
      def fpair[A](fa: F[A]): F[(A, A)]                  = map(fa)(a => (a, a))
      def mapply[A, B](a: A)(f: F[A => B]): F[B]         = map(f)((ff: A => B) => ff(a))
    }

    @typeclass trait Foldable[F[_]] {
      def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B
      def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
      def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
    }

    trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
      def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]]
      def reverse[A](fa: F[A]): F[A]
      def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])]
      def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)]
      def indexed[A](fa: F[A]): F[(Int, A)]
      def zipWithL[A, B, C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): F[C]
      def zipWithR[A, B, C](fa: F[A], fb: F[B])(f: (Option[A], B) => C): F[C]
      def mapAccumL[S, A, B](fa: F[A], z: S)(f: (S, A) => (S, B)): (S, F[B])
      def mapAccumR[S, A, B](fa: F[A], Z: S)(f: (A, S) => (B, S)): (F[B], S)
    }

    // Data Encoding of Logical OR
    sealed abstract class \&/[+A, +B]
    final case class This[A](aa: A)           extends (A \&/ Nothing)
    final case class That[B](bb: B)           extends (Nothing \&/ B)
    final case class Both[A, B](aa: A, bb: B) extends (A \&/ B)

    @typeclass trait Align[F[_]] extends Functor[F] {
      def alignWith[A, B, C](f: A \&/ B => C): (F[A], F[B]) => F[C]
      def align[A, B](a: F[A], b: F[B]): F[A \&/ B]
      def merge[A: Semigroup](a1: F[A], a2: F[A]): F[A]
      def pad[A, B]: (F[A], F[B]) => F[(Option[A], Option[B])]
      def padWith[A, B, C](f: (Option[A], Option[B]) => C): (F[A], F[B]) => F[C]
    }
  }

  object Variance {
    // Functors
    // InvariantFunctor
    @typeclass trait InvariantFunctor[F[_]] {
      def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B]
    }
    // Covariant Functor
    @typeclass trait Functor[F[_]] extends InvariantFunctor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
      override def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] = map(fa)(f)
    }

    @typeclass trait Contravariant[F[_]] extends InvariantFunctor[F] {
      def contramap[A, B](fa: F[A])(f: B => A): F[B]
      override def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] = contramap(fa)(g)
    }
  }

  object ApplyAndBind {
    import Variance._
    @typeclass trait Apply[F[_]] extends Functor[F] {
      @op("<*>") def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]
      def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: => (A, B) => C): F[C]
    }

    implicit class ApplyOps[F[_]: Apply, A](self: F[A]) {
      def *>[B](fb: F[B]): F[B] = Apply[F].apply2(self, fb)((_, b) => b)
    }
  }
}
