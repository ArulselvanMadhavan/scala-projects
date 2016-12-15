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

    def add1(l:List[Int]):List[Int] = {
        // l match {
        //   case Nil => Nil
        //   case Cons(x, xs) => Cons(x+1, add1(xs))
        // }
        List.foldRight(l, Nil:List[Int])((a, b) => Cons(a+1, b))
    }

    def dToString(l:List[Double]):List[String] = {
        // l match {
        //     case Nil => Nil
        //     case Cons(x, xs) => Cons(x.toString, dToString(xs))
        // }
        List.foldRight(l, Nil:List[String])((a, b) => Cons(a.toString, b))
    }

    def map[A, B](l:List[A])(f:A => B):List[B] = {
        // l match {
        //     case Nil => Nil
        //     case Cons(x, xs) => Cons(f(x), map(xs)(f))
        // }
        List.foldRight_optimal(l, Nil:List[B])((a, b) => Cons(f(a), b))
    }

    def filter[A](l:List[A])(f:A => Boolean):List[A] = {
        // l match {
        //     case Nil => Nil
        //     case Cons(x, xs) => {
        //         if (f(x)) Cons(x, filter(xs)(f))
        //         else filter(xs)(f)
        //     }
        // }
        List.foldRight_optimal(l, Nil:List[A])((a, b) => if (f(a)) Cons(a, b) else b)
    }

    def flatMap[A, B](l:List[A])(f:A => List[B]):List[B] = {
        // List.foldRight_optimal(l, Nil:List[B])((a, b) => List.append(f(a), b))
        concatenate(map(l)(f))
    }

    def filterViaFlatMap[A](l:List[A])(f:A => Boolean):List[A] = {
        flatMap(l)((x) => if (f(x)) List(x) else List())
    }

    def add2Lists(l1:List[Int], l2:List[Int]):List[Int] = {
        (l1, l2) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add2Lists(xs, ys))
        }
    }

    def zipWith[A](l1:List[A], l2:List[A])(f:(A,A) => A):List[A] = {
        (l1, l2) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
        }
    }

    @annotation.tailrec
    def hasSubsequence[A](sup:List[A], sub:List[A]):Boolean = {
        (sup, sub) match {
            case (Nil, Nil) => true
            case (x, Nil) => true
            case (Nil, y) => false
            case (Cons(x, xs), Cons(y, ys)) => {
                if (x == y) hasSubsequence(xs, ys)
                else hasSubsequence(xs, sub)
            }
        }
    }
}
