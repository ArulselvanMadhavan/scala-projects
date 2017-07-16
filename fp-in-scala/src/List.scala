/**
  * Created by amadhavan1 on 7/3/17.
  */
package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, ts) => ts
  }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, ts) => Cons(a, ts)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, _) if n == 0 => l
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case Cons(_, _) => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def product2(ns: List[Int]): Double =
    foldRight(ns, 1.0)(_ * _);

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product3(as: List[Int]): Double = foldLeft(as, 1.0)(_ * _)

  def length3[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def go[A](as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(x, xs) => go(xs, Cons(x, acc))
    }

    go(as, Nil: List[A])
  }

  def reverse2[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((acc, x) => f(x, acc))
  }

  def foldRight3[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((acc, x) => b => acc(f(x, b)))(z)
  }

  def foldLeft3[A, B](l: List[A], z: B)(f: (B, A) => B) = {
    foldRight(l, (b: B) => b)((a, acc) => b => acc(f(b, a)))(z)
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  def concat[A](lol: List[List[A]]): List[A] = lol match {
    case Nil => Nil
    case Cons(l, ls) => append(l, concat(ls))
  }

  def concat2[A](lol: List[List[A]]): List[A] = foldRight(lol, Nil: List[A])((l, acc) => append(l, acc))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A, B](l: List[A], f: A => B): List[B] = foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def map1[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(x, xs) => buf += f(x); go(xs)
    }

    go(l)
    List(buf.toList: _*)
  }

  def filter[A](l: List[A], f: A => Boolean): List[A] = foldLeft(l, Nil: List[A])((acc, x) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] = {
    val buf = new collection.mutable.ListBuffer[List[B]]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(x, xs) => buf += f(x); go(xs)
    }

    go(l)
    List.concat(List(buf.toList: _*))
  }

  def filter2[A, B](l: List[A], f: A => Boolean): List[A] = {
    flatMap(l, (x: A) => if (f(x)) Cons(x, Nil) else Nil: List[A])
  }

  def addzip(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addzip(xs, ys))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def isPrefixOf[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (Nil, Nil) => true
    case (Nil, _) => true
    case (_, Nil) => false
    case (Cons(x, xs), Cons(y, ys)) => if (x == y) isPrefixOf(xs, ys) else false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val result = foldRight(sup, Nil: List[List[A]])((x: A, lol) => lol match {
      case Nil => Cons(Cons(x, Nil), lol)
      case Cons(l1, _) => Cons(Cons(x, l1), lol)
    })
    val filteredList = List.filter(result, (l: List[A]) => isPrefixOf(sub, l))
    List.length(filteredList) > 0
  }
}
