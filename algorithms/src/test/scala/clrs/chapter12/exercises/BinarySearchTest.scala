package clrs.chapter12.exercises

import org.scalatest.{FunSuite, Matchers}

class BinarySearchTreeTests extends FunSuite with Matchers {
  test("should construct a binarysearchtree properly") {
    val t1 = BinarySearchTree(9, 2, 5, 1, 8)
    println(t1)
    val isValid = BinarySearchTree.isValid(t1)
    println(isValid)
  }
}
