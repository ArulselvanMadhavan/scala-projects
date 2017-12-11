package clrs.chapter12.exercises

import org.scalatest.{FunSuite, Matchers}

class BinarySearchTreeTests extends FunSuite with Matchers {
  test("should construct a binarysearchtree properly") {
    val t1      = BinarySearchTree(9, 2, 5, 1, 8)
    val isValid = BinarySearchTree.isValid(t1)
    isValid should be(true)
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
  test("should return a list of elements traversed inorder") {
    val b1 = BinarySearchTree(9, 4, 15, 2, 3, 12, 16, 1)
    val ll = BinarySearchTree.traversal(b1)
    ll should be(List(9, 4, 2, 1, 3, 15, 12, 16))
  }
  test("should return a list of elements by their height level") {
    val b1     = BinarySearchTree(13, 10, 15, 8, 12, 7, 9, 11, 14, 16)
    val ll     = BinarySearchTree.levelOrderTraversal(b1)
    val result = List(List(13), List(10, 15), List(8, 12, 14, 16), List(7, 9, 11))
    ll should be(result)
  }
}
