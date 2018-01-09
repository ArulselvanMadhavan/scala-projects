package hackerrank.graph
import clrs.utils.Graph
import java.util.Scanner

object RoadsAndLibraries {

  private[this] def buildGraph(sc: Scanner,
                               cities: Int,
                               roads: Int,
                               c_road: Int): Graph[Int, Int] = {
    val a = Array.fill(cities, cities)(0)
    for {
      r <- 0 until roads
      i = sc.nextInt() - 1
      j = sc.nextInt() - 1
    } a(i)(j) = c_road
    Graph.buildGraphFromMatrix(a)
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    for {
      i <- 0 until sc.nextInt()
    } yield {
      val cities = sc.nextInt()
      val roads  = sc.nextInt()
      val c_lib  = sc.nextInt()
      val c_road = sc.nextInt()
      val g      = buildGraph(sc, cities, roads, c_road)
      println(s"${g}\t${c_lib}")
    }
  }
}
