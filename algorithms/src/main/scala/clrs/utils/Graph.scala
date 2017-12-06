package clrs.utils
import scala.collection.immutable.Queue

object EdgeType extends Enumeration {
  type EdgeType = Value
  val Tree, Back, Forward, Cross = Value
}

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val Processing, Finished = Value
}

final case class Connection[E,N : Ordering](from:Graph[E,N], to:Graph[E,N])
case class Edge[E, N : Ordering](from: Graph[E, N], to: Graph[E, N], weight: E)

class Graph[E, N : Ordering](var value: N = null.asInstanceOf[N]) {
  import EdgeType._
  import NodeStatus._
  type TraversalStats = (Map[Graph[E,N], NodeStats],Map[Connection[E,N], EdgeType], Int)
  type NodeStats = (Int, NodeStatus)
  var inEdges: List[Edge[E, N]]  = Nil
  var outEdges: List[Edge[E, N]] = Nil

  def succs: List[Graph[E, N]] = outEdges.map(_.to)

  def preds: List[Graph[E, N]] = inEdges.map(_.from)

  def allNodes:List[Graph[E,N]] = succs ::: preds

  def bfs(directed:Boolean = true): List[(Graph[E, N], Int)] = {
    def loop(q: Queue[Graph[E, N]], m: Map[Graph[E, N], Int], id: Int): Map[Graph[E, N], Int] = {
      if(q.isEmpty) m
      else if (m.contains(q.head) == false) {
        val g                      = q.head
        val nextWave               = if(directed) g.succs else g.allNodes
        val qq: Queue[Graph[E, N]] = q.tail ++ nextWave
        loop(qq, m + (g -> id), id + 1)
      } else loop(q.tail, m, id)

    }
    loop(Queue(this), Map[Graph[E, N], Int](), 0).toList
  }

  def dfsClassifyEdges:List[(Connection[E,N], EdgeType)] = {

    def loop(g:Graph[E,N], mg:Map[Graph[E,N], NodeStats], me:Map[Connection[E,N], EdgeType], id:Int, src:Graph[E, N]): TraversalStats = {
      if(mg.contains(g) == false) {
        val mmg:Map[Graph[E,N], NodeStats] = mg + (g -> new Tuple2(id, Processing))
        val mme = me + (Connection(src, g) -> Tree)
        val start:TraversalStats = (mmg, mme, id + 1)
        val successors = g.succs.sortBy(_.value)
        val result = successors.foldLeft(start)((acc, gg) => loop(gg, acc._1, acc._2, acc._3, g))
        (result._1 + (g -> new Tuple2(id, Finished)), result._2, result._3)
      } else {
        (mg(src), mg(g)) match {
          case ((u, Processing), (v,Processing)) if v < u => (mg, me + (Connection(src, g) -> Back), id)
          case ((u, Processing), (v, Finished)) if u < v => (mg, me + (Connection(src, g) -> Forward), id)
          case ((u, Processing), (v, Finished)) if v < u => (mg, me + (Connection(src, g) -> Cross), id)
        }
      }
    }
    loop(this, Map[Graph[E,N], NodeStats](), Map[Connection[E,N], EdgeType](), 0, this)._2.toList
  }
  //1. Forward Edge. On First discovery
  def dfs: List[(Graph[E, N], Int)] = {
    def loop(id: Int, g: Graph[E, N], m: Map[Graph[E, N], Int]): (Map[Graph[E, N], Int], Int) = {
      if (m.contains(g) == false) {
        g.succs.foldLeft((m + (g -> id), id + 1))((acc, gg) => loop(acc._2, gg, acc._1))
      } else (m, id)
    }
    val result = loop(0, this, Map[Graph[E, N], Int]())
    result._1.toList
  }

  def graphsByDepth: List[Graph[E, N]] = {
    def loop(g: Graph[E, N], s: Set[Graph[E, N]]): Set[Graph[E, N]] =
      if (!s(g)) {
        val ss = g.succs.foldLeft(s + g)((acc, gg) => loop(gg, acc))
        g.preds.foldLeft(ss)((acc, gg) => loop(gg, acc))
      } else s

    loop(this, Set()).toList
  }

  def connect(from: N, via: E, to: N): (Graph[E, N], Graph[E, N]) = {
    val fromGraph: Graph[E, N] = if (value == null) { value = from; this } else
      hop(from) match {
        case Some(g) => g
        case None    => Graph.one(from)
      }

    val toGraph: Graph[E, N] = hop(to) match {
      case Some(g) => g
      case None    => Graph.one(to)
    }

    fromGraph.outEdges = new Edge(fromGraph, toGraph, via) :: fromGraph.outEdges
    toGraph.inEdges = new Edge(fromGraph, toGraph, via) :: toGraph.inEdges
    (fromGraph, toGraph)
  }

  def hop(n: N): Option[Graph[E, N]] = graphsByDepth.find(_.value == n)

  override def equals(o: Any): Boolean = o match {
    case that: Graph[_, _] => (that canEqual this) && value == that.value
    case _                 => false
  }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Graph[E, N]]

  override def hashCode: Int = (value).##

  override def toString: String = "Graph(" + value + ")"
}

object Graph {

  def apply[E, N : Ordering](tuples: (N, E, N)*): Graph[E, N] = {
    val g: Graph[E, N] = Graph.empty
    for ((from, via, to) <- tuples) {
      g.connect(from, via, to)
    }
    g
  }

  def one[E, N : Ordering](n: N): Graph[E, N] = new Graph(n)

  def empty[E, N : Ordering]: Graph[E, N] = new Graph

}
