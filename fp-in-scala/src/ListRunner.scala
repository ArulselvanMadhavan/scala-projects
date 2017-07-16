/**
  * Created by amadhavan1 on 7/4/17.
  */

import fpinscala.datastructures.List
import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil
import fpinscala.datastructures.Branch
import fpinscala.datastructures.Leaf
import fpinscala.datastructures.Tree

object ListRunner {
  def main(args: Array[String]): Unit = {
    println(Cons(2, Cons(3, Nil)))
    val xs: List[Int] = List(2, 2, 20, 0, 7, 90);
    val l1 = Leaf(1)
    val l2 = Leaf(2)
    val l3 = Leaf(3)
    val l4 = Leaf(4)
    val l5 = Leaf(5)
    val t1 = Branch(l1, l2)
    val t2 = Branch(l4, l5)
    val t3 = Branch(l3, t2)
    val t:Tree[Int] = Branch(t1, t3)
    println(List.tail(xs));
    println(List.setHead(5, xs));
    println(List.drop(xs, 2))
    println(List.drop(xs, 0))
    println(List.drop(xs, 5))
    println(List.drop(xs, 4))
    println(List.dropWhile(xs)((x) => x % 2 == 0))
    println(List.append(xs, List(22, 44, 66)))
    println(List.init(xs))
    println(List.product2(xs))
    println(List.length(xs))
    println(List.foldLeft(xs, 0)(_ + _))
    println(List.reverse(xs))
    println(List.reverse2(xs))
    println(List.foldRight3(xs, 0)(_ + _))
    println(List.append2(xs, List(3, 3, 21, 1, 7, 91)))
    println(List.concat(List(xs, List(3, 3, 21, 1, 7, 91))))
    println(List.filter(xs, (x:Int) => (x % 2 == 1)))
    println(List.flatMap(xs, (x:Int) => List(4, x)))
    println(List.filter2(xs, (x:Int) => (x % 2 == 1)))
    println(List.addzip(xs, List(4, 5, 6)))
    println(List.zipWith(xs, List(4, 5, 6))(_ * _))
    println(List.hasSubsequence(xs, List(7, 90)))
    println(Tree.size(t) == Tree.size2(t))
    println(Tree.maximum(t) == Tree.maximum2(t))
    println(Tree.depth(t) == Tree.depth2(t))
    println(Tree.map(t, (x:Int) => x * 2.0) == Tree.map2(t, (x:Int) => x * 2.0))
  }
}
