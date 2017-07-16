/**
  * Created by amadhavan1 on 7/16/17.
  */
package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum[A](t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  def fold[A, B](t: Tree[A], leafFunc: A => B)(combiner: (B, B) => B): B = {
    def go[A, B](x: Tree[A], g: A => B)(z: (B, B) => B): B = x match {
      case Leaf(lf) => g(lf)
      case Branch(l, r) => z(go(l, g)(z), go(r, g)(z))
    }

    go(t, leafFunc)(combiner)
  }

  def size2[A](t: Tree[A]): Int = fold(t, (_: A) => 1)((ls: Int, rs: Int) => 1 + ls + rs)
  def maximum2(t:Tree[Int]): Int = fold(t, (x:Int) => x)((ls:Int, rs:Int) => ls max rs)
  def depth2[A](t:Tree[A]):Int = fold(t, (_: A) => 1)((ls:Int, rs:Int) => 1 + (ls max rs))
  def map2[A, B](t:Tree[A], f:A => B):Tree[B] = fold(t, (x:A) => Leaf(f(x)):Tree[B])((ls:Tree[B], rs:Tree[B]) => Branch(ls, rs))
}