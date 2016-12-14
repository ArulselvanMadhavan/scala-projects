package fp.functional_data_structures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {
    def sum(ints:List[Int]):Int = {
        ints match {
            case Nil => 0
            case Cons(x, xs) => x + sum(xs)
        }
    }

    def product(ds:List[Double]):Double = {
        ds match {
            case Nil => 1.0
            case Cons(0.0, _) => 0.0
            case Cons(x, xs) => x * product(xs)
        }
    }

    def apply[A](as: A*):List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail:_*))
    }

    def tail[A](xs:List[A]):Option[List[A]] = {
        xs match {
            case Nil => None
            case Cons(x, xs) => new Some(xs)
        }
    }

    def setHead[A](xs:List[A], y:A):List[A] = {
        xs match {
            case Nil => Cons(y, Nil)
            case Cons(x, xs) => Cons(y, xs)
        } 
    }

    def drop[A](l:List[A], n:Int):List[A] = {
        @annotation.tailrec
        def helper(l:List[A], n:Int):List[A] = {
            if (n == 0) l
            else {
                val next = List.tail(l) match {
                    case None => (Nil, 0)
                    case Some(l) => (l, n-1)
                };
                helper(next._1, next._2)
            }
        }
        helper(l, n)
    }

    def head[A](l:List[A]):Option[A] = {
        l match {
            case Nil => None
            case Cons(x, xs) => Some(x)
        } 
    }

    def checkFirst[A](l:List[A], f:A => Boolean):Boolean = {
        List.head(l) match {
            case None => false
            case Some(x) => f(x)
        }
    }

    def dropWhile[A](l:List[A], f:A => Boolean):List[A] = {
        @annotation.tailrec
        def helper(l:List[A]):List[A] = {
            if (List.checkFirst(l, f)) helper(List.drop(l, 1))
            else l
        }
        helper(l)
    }

    def init[A](l:List[A]):List[A] = {
        l match {
            case Nil => Nil
            case Cons(x, Nil) => Nil
            case Cons(x, xs) => Cons(x, init(xs))
        }
    }

    def foldRight[A, B](l:List[A], z:B)(f:(A, B) => B):B = {
        l match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }       
    }

    def foldRight_optimal[A, B](l:List[A], z:B)(f:(A, B) => B):B = {
        List.foldLeft(List.reverse(l), z)((b, a) => f(a, b))
    }

    @annotation.tailrec
    def foldLeft[A, B](l:List[A], z:B)(f:(B, A) => B): B = {
        l match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
    }

    def reverse[A](l:List[A]):List[A] = {
        List.foldLeft(l, Nil:List[A])((z:List[A], y:A) => Cons(y, z))
    }

    def append[A](l1:List[A], l2:List[A]):List[A] = {
        List.foldRight(l1, l2)((a,b) => Cons(a, b))
    }

    def length[A](l:List[A]):Int = {
        List.foldLeft(l, 0)((x, y) => 1 + x)
    }

    def concatenate[A](ll:List[List[A]]):List[A] = {
        val len:Int = List.length(ll) 
        List.foldRight_optimal(ll, Nil:List[A])((a, b) => List.append(a, b))
    }
}