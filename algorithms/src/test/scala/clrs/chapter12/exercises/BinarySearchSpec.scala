package clrs.chapter12.exercises

import org.scalatest.{FunSuite, Matchers}

class BinarySearchTreeTests extends FunSuite with Matchers {
  test("should construct a binarysearchtree properly") {
    val t1 = BinarySearchTree(9, 2, 5, 1, 8)
    // println(t1)
    val isValid = BinarySearchTree.isValid(t1)
    // println(isValid)
  }
  test("should find the least common ancestor of any BST") {
    val b1   = BinarySearchTree(6, 2, 8, 0, 4, 3, 5, 7, 9)
    val lca1 = BinarySearchTree.lca(b1)(2, 8)
    lca1 should be(Some(6))
    val lca2 = BinarySearchTree.lca(b1)(2, 4)
    lca2 should be(Some(2))
    val lca3 = BinarySearchTree.lca(b1)(4, 2)
    lca3 should be(Some(2))
  }
}
