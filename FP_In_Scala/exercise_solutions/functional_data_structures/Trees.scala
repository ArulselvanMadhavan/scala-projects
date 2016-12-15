package fp.functional_data_structures;

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {
    def size[A](t:Tree[A]):Int = {
        t match {
            case Leaf(_) => 1
            case Branch(l,r) => 1 + size(l) + size(r)
        }
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(n) => n
        case Branch(l,r) => maximum(l) max maximum(r)
    }

    def depth[A](t:Tree[A]):Int = {
        t match {
            case Leaf(n) => 0
            case Branch(l, r) => (depth(l) max depth(r)) + 1
        }
    }

    def map[A, B](t:Tree[A])(f:A => B):Tree[B] = {
        t match {
            case Leaf(n) => Leaf(f(n))
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        }
    }

    def fold[A, B](t:Tree[A])(i:A => B)(f:(B, B) => B):B = {
        t match {
            case Leaf(n) => i(n)
            case Branch(l, r) => {
                val a:B = fold(l)(i)(f)
                val b:B = fold(r)(i)(f)
                f(a, b)
            }
        }
    }

    def sizeviaFold[A](t:Tree[A]):Int = {
        Tree.fold(t)(_ => 1)((l, r) => l + r + 1)
    }

    def maximumviaFold(t:Tree[Int]):Int = {
        fold(t)(n => n)(_ max _)
    }

    def depthviaFold[A](t:Tree[A]):Int = {
        fold(t)(_ => 0)((l, r) => 1 + (l max r))
    }

    def mapviaFold[A, B](t:Tree[A])(f:A => B):Tree[B] = {
        fold(t)(n => Leaf(f(n)):Tree[B])((l, r) => Branch(l, r))
    }
}