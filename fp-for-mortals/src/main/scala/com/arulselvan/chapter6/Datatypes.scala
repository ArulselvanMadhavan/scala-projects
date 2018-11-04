package com.arulselvan.chapter6

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
        def value = value0
      }
    }

    final case class Value[A](value: A) extends Need[A]
  }

  object Tagging {
    type @@[A, T] = Tag.k.@@[A, T]

    object Tag {
      @inline val k: Tagkind = IdTagkind
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
}
