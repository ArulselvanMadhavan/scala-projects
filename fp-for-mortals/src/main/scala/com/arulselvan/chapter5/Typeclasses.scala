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
      // def msuml[G[_]: PlusEmpty, A](fa: F[G[A]]): G[A]
      // def collapse[X[_]: ApplicativePlus, A](x : F[A]): X[A]
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
      def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C]
    }

    class ApplicativeBuilder[F[_]: Apply, A, B](a: F[A], b: F[B]) {
      def tupled: F[(A, B)] = Apply[F].apply2(a, b) {
        case (a, b) => Tuple2(a, b)
      }
      def |@|[C](fc: F[C]): ApplicativeBuilder3[C] = ???
      sealed abstract class ApplicativeBuilder3[C](c: F[C])
    }

    implicit class ApplyOps[F[_]: Apply, A](self: F[A]) {
      def *>[B](fb: F[B]): F[B]                         = Apply[F].apply2(self, fb)((_, b) => b)
      def <*[A](fa: F[A]): F[A]                         = Apply[F].apply2(fa, self)((a, _) => a)
      def |@|[B](fb: F[B]): ApplicativeBuilder[F, A, B] = new ApplicativeBuilder(self, fb)
    }

    @typeclass trait Bind[F[_]] extends Apply[F] {
      @op(">>=") def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = bind(fa)(f)

      override def ap[A, B](fa: => F[A])(ff: => F[A => B]): F[B] =
        bind(ff)(f => map(fa)(f))

      override def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
        bind(fa)(a => map(fb)(b => f(a, b)))

      def join[A](ffa: F[F[A]]): F[A] =
        bind(ffa)(identity)

      def mproduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)] =
        bind(fa)(a => map(f(a))(b => (a, b)))

      def ifM[B](value: F[Boolean], t: => F[B], f: => F[B]): F[B] =
        bind(value)(v => if (v) t else f)
    }

    implicit class BindOps[F[_]: Bind, A](self: F[A]) {
      // Discards the input to bind.
      def >>=[B](b: => F[B]): F[B] = Bind[F].bind(self)(_ => b)
      // Runs the effect but discards the output.
      def >>![B](f: A => F[B]): F[A] = Bind[F].bind(self)(a => Bind[F].map(f(a))(_ => a))
    }
  }

  object ApplicativeAndBind {
    import ApplyAndBind._
    // Laws
    // 1. Identity
    // 2. Homomorphism
    // 3. Interchange
    // 4. Mappy
    @typeclass trait Applicative[F[_]] extends Apply[F] {
      def point[A](a: => A): F[A]
      def pure[A](a: => A): F[A] = point(a)
    }

    // Laws
    // 1. Left Identity
    // 2. Right Identity
    // 3. Associativity
    @typeclass trait Monad[F[_]] extends Applicative[F] with Bind[F]
  }

  object DivideAndConquer {
    import Variance._

    // Divide is the contravariant equivalent of Apply.
    @typeclass trait Divide[F[_]] extends Contravariant[F] {
      def divide[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C] =
        divide2(fa, fb)(f)

      def divide2[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C]
    }

    // Divide's Contravariant equivalent of Applicative
    @typeclass trait Divisible[F[_]] extends Divide[F] {
      def conquer[A]: F[A]
    }
  }

  object PlusFamily {
    import ApplicativeAndBind._
    // Plus is semigroup for type constructors.
    @typeclass trait Plus[F[_]] {
      @op("<+>") def plus[A](a: F[A], b: => F[A]): F[A]
    }

    // PlusEmpty is Monoid for type constructors
    @typeclass trait PlusEmpty[F[_]] extends Plus[F] {
      def empty[A]: F[A]
    }
    // Useful for querying F[A] and check if it's empty.
    @typeclass trait IsEmpty[F[_]] extends PlusEmpty[F] {
      def isEmpty[A](fa: F[A]): Boolean
    }

    @typeclass trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F]
    @typeclass trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] {
      def unite[T[_]: Foldable, A](ts: F[T[A]]): F[A]
      def withFilter[A](fa: F[A])(f: A => Boolean): F[A]
    }
  }

  object ZipFamily {
    @typeclass trait Zip[F[_]] {
      def zip[A, B](a: => F[A], b: => F[B]): F[(A, B)]
      def zipWith[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C)(implicit F: Functor[F]): F[C]
      def ap(implicit F: Functor[F]): Apply[F]
      @op("<*|*>") def apzip[A, B](f: => F[A] => F[B], a: => F[A]): F[(A, B)]
    }

    @typeclass trait Unzip[F[_]] {
      @op("unfzip") def unzip[A, B](a: F[(A, B)]): (F[A], F[B])
      def firsts[A, B](a: F[(A, B)]): F[A]
      def seconds[A, B](a: F[(A, B)]): F[B]
      def unzip3[A, B, C](x: F[(A, (B, C))]): (F[A], F[B], F[C])
    }
  }

  object OptionalFamily {

    sealed abstract class Maybe[A]
    final case class Empty[A]()    extends Maybe[A]
    final case class Just[A](a: A) extends Maybe[A]

    @typeclass trait Optional[F[_]] {
      def pextract[B, A](fa: F[A]): F[B] \/ A
      def getOrElse[A](fa: F[A])(default: => A)
      def orElse[A](fa: F[A])(alt: => F[A]): F[A]
      def isDefined[A](fa: F[A]): Boolean
    }

    implicit class OptionalOps[F[_]: Optional, A](fa: F[A]) {
      def ?[X](some: => X): Conditional[X] = new Conditional[X](some)

      final class Conditional[X](some: => X) {
        def |(none: => X): X = if (Optional[F].isDefined(fa)) some else none
      }
    }
  }

  object CoThings {
    import Variance._
    import OptionalFamily._

    // Co-thing has opposite type signature to whatever thing does.
    @typeclass trait Cobind[F[_]] extends Functor[F] {
      // def bind[A, B](fa: F[A])(f: A => F[B]):F[B]
      def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]
      // def join[A](fa: F[F[A]]):F[A]
      def cojoin[A](fa: F[A]): F[F[A]]
    }

    @typeclass trait Comonad[F[_]] extends Cobind[F] {
      def copoint[A](p: F[A]): A
      // def point[A](a: =>A):F[A]
    }

    final case class Hood[A](lefts: IList[A], focus: A, rights: IList[A])

    object Hood {
      implicit class Ops[A](hood: Hood[A]) {
        def toIList: IList[A] = hood.lefts.reverse ::: hood.focus :: hood.rights
        def previous: Maybe[Hood[A]] = hood.lefts match {
          case INil()            => Empty()
          case ICons(head, tail) => Just(Hood(tail, head, hood.focus :: hood.rights))
        }
        def next: Maybe[Hood[A]] = hood.rights match {
          case INil()            => Empty()
          case ICons(head, tail) => Just(Hood(hood.focus :: hood.lefts, head, tail))
        }

        def more(f: Hood[A] => Maybe[Hood[A]]): IList[Hood[A]] =
          f(hood) match {
            case Empty() => INil()
            case Just(r) => ICons(r, r.more(f))
          }
        def positions: Hood[Hood[A]] = {
          val left  = hood.more(_.previous)
          val right = hood.more(_.next)
          Hood(left, hood, right)
        }

        implicit val comonad: Comonad[Hood] = new Comonad[Hood] {
          def map[A, B](fa: Hood[A])(f: A => B): Hood[B] =
            Hood(fa.lefts.map(f), f(fa.focus), fa.rights.map(f))
          def cobind[A, B](fa: Hood[A])(f: Hood[A] => B): Hood[B] =
            map(fa.positions)(f)
          def copoint[A](fa: Hood[A]): A =
            fa.focus
          def cojoin[A](fa: Hood[A]): Hood[Hood[A]] =
            fa.positions
        }
      }
    }

    @typeclass trait Cozip[F[_]] {
      // def zip[A, B](a: =>F[A], b: =>F[B]):F[(A, B)]
      def cozip[A, B](x: F[A \/ B]): F[A] \/ F[B]
    }
  }

  object BiThings {
    @typeclass trait Bifunctor[F[_, _]] {
      def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
      @op("<-:") def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B]
      @op(":->") def rightMap[A, B, C](fab: F[A, B])(f: B => C): F[A, C]
      @op("<:>") def umap[A, B](fab: F[A, B])(f: A => B): F[B, B]
    }
    @typeclass trait Bifoldable[F[_, _]] {
      def bifoldMap[A, B, M: Monoid](fa: F[A, B])(f: A => M)(g: B => M): M
      def bifoldRight[A, B, C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C
      def bifoldLeft[A, B, C](fa: F[A, B], z: C)(f: (C, A) => C)(g: (C, B) => C): C
      def bifoldMap1[A, B, M: Semigroup](fa: F[A, B])(f: A => M)(g: B => M): Option[M]
    }
    @typeclass trait Bitraverse[F[_, _]] extends Bifunctor[F] with Bifoldable[F] {
      def bitraverse[G[_]: Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C])(
          g: B => G[D]): G[F[C, D]]
      def bisequence[G[_]: Applicative, A, B](x: F[G[A], G[B]]): G[F[A, B]]
    }
    @typeclass trait MonadPlus[F[_]] {
      def separate[G[_, _]: Bifoldable, A, B](value: F[G[A, B]]):(F[A], F[B])
    }
  }

}
