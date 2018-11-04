package com.arulselvan.chapter6

import scalaz._
import Scalaz._

object DataTypes {

  object Variances {
    sealed abstract class Liskov[-A, +B] {
      def apply(a: A): B
      def subst[F[- _]](p: F[B]): F[A]
      def andThen[C](that: Liskov[B, C]): Liskov[A, C]
      def onF[X](fa: X => A): X => B
    }
    object Liskov {
      type <~<[-A, +B] = Liskov[A, B]
      type >~>[+B, -A] = Liskov[A, B]
      implicit def refl[A]: (A <~< A)                 = ???
      implicit def isa[A, B >: A]: A <~< B            = ???
      implicit def witness[A, B](lt: A <~< B): A => B = ???
    }

    sealed abstract class Leibniz[A, B] {
      def apply(a: A): B
      def subst[F[_]](p: F[A]): F[B]
      def flip: Leibniz[B, A]
      def andThen[C](that: Leibniz[B, C]): Leibniz[A, C]
      def onF[X](fa: X => A): X => B
    }

    object Leibniz {
      type ===[A, B] = Leibniz[A, B]
      implicit def refl[A]: Leibniz[A, A]                    = ???
      implicit def subst[A, B](a: A)(implicit f: A === B): B = ???
      implicit def witness[A, B](f: A === B): A => B         = ???
    }
  }

  object Evaluation {
    sealed abstract class Name[A] {
      def value: A
    }

    object Name {
      def apply[A](a: => A) = new Name[A] {
        def value: A = a
      }
    }

    sealed abstract class Need[A] extends Name[A]
    object Need {
      def apply[A](a: => A): Need[A] = new Need[A] {
        private lazy val value0: A = a
        def value                  = value0
      }
    }

    final case class Value[A](value: A) extends Need[A]
  }

  object Tagging {
    type @@[A, T] = Tag.k.@@[A, T]

    object Tag {
      @inline val k: Tagkind                = IdTagkind
      @inline def apply[A, T](a: A): A @@ T = k(a)
    }

    sealed abstract class Tagkind {
      type @@[A, T]
      def apply[A, T](a: A): A @@ T
    }

    private[this] object IdTagkind extends Tagkind {
      type @@[A, T] = A
      @inline override def apply[A, T](a: A): A = a
    }
  }

  object Transformations {
    type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
    trait NaturalTransformation[-F[_], +G[_]] {
      def apply[A](fa: F[A]): G[A]
      def compose[E[_]](f: E ~> F): E ~> G
      def andThen[H[_]](f: G ~> H): F ~> H
    }
  }

  object Isomorphism {

    trait Iso[Arr[_, _], A, B] {
      def to: Arr[A, B]
      def from: Arr[B, A]
    }

    type IsoSet[A, B] = Iso[Function1, A, B]
    type <=>[A, B]    = IsoSet[A, B]

    object IsoSet {
      def apply[A, B](to: A => B, from: B => A): A <=> B = ???
    }

    trait Iso2[Arr[_[_], _[_]], F[_], G[_]] {
      def to: Arr[F, G]
      def from: Arr[G, F]
    }

    type IsoFunctor[F[_], G[_]] = Iso2[NaturalTransformation, F, G]
    type <~>[F[_], G[_]]        = IsoFunctor[F, G]

    object IsoFunctor {
      def apply[F[_], G[_]](to: F ~> G, from: G ~> F): F <~> G = ???
    }

    trait Iso3[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] {
      def to: Arr[F, G]
      def from: Arr[G, F]
    }

    type IsoBiFunctor[F[_, _], G[_, _]] = Iso3[~~>, F, G]
    type <~~>[F[_, _], G[_, _]]         = IsoBiFunctor[F, G]

  }

  object Containers {
    sealed abstract class Maybe[A] {
      import Maybe._
      def cata[B](f: A => B, b: => B): B = this match {
        case Just(a) => f(a)
        case Empty() => b
      }
      def |(a: => A): A                    = cata(identity, a)
      def toLeft[B](b: => B): A \/ B       = cata(\/.left, \/-(b))
      def toRight[B](b: => B): B \/ A      = cata(\/.right, -\/(b))
      def <\/[B](b: => B): A \/ B          = toLeft(b)
      def \/>[B](b: => B): B \/ A          = toRight(b)
      def orZero(implicit A: Monoid[A]): A = ??? // getOrElse(A.zero)
      def orEmpty[F[_]: Applicative: PlusEmpty]: F[A] =
        cata(Applicative[F].point(_), PlusEmpty[F].empty)
    }
    object Maybe {
      final case class Empty[A]()    extends Maybe[A]
      final case class Just[A](a: A) extends Maybe[A]

      def empty[A]: Maybe[A]      = Empty()
      def just[A](a: A): Maybe[A] = Just(a)
    }

    implicit class MaybeOps[A](self: A) {
      def just: Maybe[A] = Maybe.just(self)
    }
  }

  object ValidationFamily {
    // Doesn't have monad instance. Has Applicative, Traverse/BiTraverse, Cozip, Plus, Optional
    sealed abstract class Validation[+E, +A]
    final case class Success[A](a: A) extends Validation[Nothing, A]
    final case class Failure[E](e: E) extends Validation[E, Nothing]
    type ValidationNel[E, +X] = Validation[NonEmptyList[E], X]

    object Validation {
      type \?/[+E, +A] = Validation[E, A]

      def success[E, A]: A => Validation[E, A]                               = Success(_)
      def failure[E, A]: E => Validation[E, A]                               = Failure(_)
      def failureNel[E, A](e: E): ValidationNel[E, A]                        = Failure(NonEmptyList(e))
      def lift[E, A](a: A): ValidationNel[E, A]                              = ???
      def liftNel[E, A](a: A)(f: A => Boolean, fail: E): ValidationNel[E, A] = ???
      def fromEither[E, A](e: Either[E, A]): Validation[E, A]                = ???
    }

    implicit class ValidationOps[A](self: A) {
      def success[X]: Validation[X, A]       = Validation.success[X, A](self)
      def successNel[X]: ValidationNel[X, A] = success
      def failure[X]: Validation[A, X]       = Validation.failure[A, X](self)
      def failureNel[X]: ValidationNel[A, X] = Validation.failureNel[A, X](self)
    }
  }

  object TheseFamily {
    // A data encoding of inclusive logical OR.
    sealed abstract class \&/[+A, +B]
    object \&/ {
      type These[A, B] = A \&/ B
      final case class This[A](aa: A)           extends (A \&/ Nothing)
      final case class That[B](bb: B)           extends (Nothing \&/ B)
      final case class Both[A, B](aa: A, bb: B) extends (A \&/ B)

      def apply[A, B](a: A, b: B): These[A, B] = Both(a, b)
    }

    implicit class TheseOps[A](self: A) {
      final def wrapThis[B]: A \&/ B = \&/.This(self)
      final def wrapThat[B]: B \&/ A = \&/.That(self)
    }
    implicit class ThesePairOps[A, B](self: (A, B)) {
      final def both: A \&/ B = \&/.Both(self._1, self._2)
    }

    final case class Coproduct[F[_], G[_], A](run: F[A] \/ G[A])
    object Coproduct {
      def leftc[F[_], G[_], A](x: F[A]): Coproduct[F, G, A]  = Coproduct(-\/(x))
      def rightc[F[_], G[_], A](x: G[A]): Coproduct[F, G, A] = Coproduct(\/-(x))
    }
  }

  object ConstThings {
    final case class Const[A, B](getConst: A)

    implicit def applicative[A: Monoid]: Applicative[Const[A, ?]] =
      new Applicative[Const[A, ?]] {
        def point[B](b: => B): Const[A, B] =
          Const(Monoid[A].zero)
        def ap[B, C](fa: => Const[A, B])(fbc: => Const[A, B => C]): Const[A, C] =
          Const(fbc.getConst |+| fa.getConst)
      }
  }

  object CollectionsFamily {
    sealed abstract class IList[A] {
      def ::(a: A): IList[A]          = ???
      def :::(as: IList[A]): IList[A] = ???
      def toList: List[A]             = ???
    }

    final case class INil[A]()                         extends IList[A]
    final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
    final case class NonEmptyList[A](head: A, tail: IList[A])

    sealed abstract class EphemeralStream[A] {
      def headOption: Option[A]
      def tailOption: Option[EphemeralStream[A]]
    }

    object EphemeralStream {
      type EStream[A] = EphemeralStream[A]
      def emptyEphemeralStream[A]: EStream[A]                           = ???
      def cons[A](a: => A, as: => EStream[A]): EStream[A]               = ???
      def unfold[A, B](start: => B)(f: B => Option[(A, B)]): EStream[A] = ???
      def iterate[A](start: A)(f: A => A): EStream[A]                   = ???

      implicit class ConsWrap[A](e: => EStream[A]) {
        def ##::(h: A): EStream[A] = cons(h, e)
      }

      implicit class EStreamWrapper[A : Monoid](xs:EStream[A]) {
        def isEmpty:Boolean = xs.headOption match {
          case Some(_) => false
          case None => true
        }
        def head():A = xs.headOption match {
          case Some(x) => x
          case None => Monoid[A].zero
        }
        def tail():EStream[A] = xs.tailOption match {
          case Some(t) => t
          case None => emptyEphemeralStream
        }
      }
      object ##:: {
        def unapply[A : Monoid](xs: EStream[A]):Option[(A, EStream[A])] =
          if (xs.isEmpty) None
          else Some((xs.head(), xs.tail()))
      }
    }

    object CoRecursiveThings {
      sealed abstract class CoRecursiveList[A] {
        type S
        def init: S
        def step: S => Maybe[(S, A)]
      }

      object CoRecursiveList {
        private final case class CoRecursiveListImpl[S0, A](
          init: S0,
          step: S0 => Maybe[(S0, A)]
        ) extends CoRecursiveList[A] { type S = S0 }
      }
    }
  }
}
