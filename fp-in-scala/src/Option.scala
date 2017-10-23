/**
  * Created by amadhavan1 on 7/16/17.
  */
package fpinscala.errorhandling

import scala.{Option => _, Either => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap ((x: A) => if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def go[A](a: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = a match {
      case Nil => acc
      case None :: xs => None
      case Some(x) :: xs => go(xs, acc.map((l: List[A]) => x :: l))
    }

    go(a, Some(List())).map((l: List[A]) => l.reverse)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap ((xx: A) => sequence2(xs) map (xx :: _))
  }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil: List[B]))((x: A, acc: Option[List[B]]) => map2(f(x), acc)(_ :: _))
  }

  def sequence4[A](a:List[Option[A]]):Option[List[A]] = {
    traverse(a)((x:Option[A]) => x)
  }
}