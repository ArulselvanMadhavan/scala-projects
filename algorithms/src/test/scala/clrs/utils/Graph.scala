package clrs.utils

import org.scalatest.{Matchers, FunSuite, BeforeAndAfter}

class GraphTest extends FunSuite with Matchers with BeforeAndAfter {
  var g: Graph[Int, String] = _
  before {
    g = Graph(("A", 20, "B"),
              ("B", 30, "C"),
              ("B", 60, "E"),
              ("C", 40, "D"),
              ("D", 50, "E"),
              ("E", 60, "A"))

  }
  test("should return run dfs using outgoing edges") {
    // println(g.dfs)
    println(g.bfs())
    println(g.bfs(false))
  }
  test("should run dfs and classify edges") {
    println(g.dfsClassifyEdges)
  }
}
