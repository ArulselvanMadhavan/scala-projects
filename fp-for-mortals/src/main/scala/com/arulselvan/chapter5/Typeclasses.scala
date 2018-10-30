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

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())
  }

}
