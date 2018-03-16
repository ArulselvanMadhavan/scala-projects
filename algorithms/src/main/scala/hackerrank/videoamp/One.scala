package hackerrank.videoamp
import scala.collection.mutable.Map

object One {

  def fillMap(s1: String): Map[Char, Int] = {
    val m: Map[Char, Int] = Map()
    for {
      c <- s1.toLowerCase.toCharArray
    } {
      val count: Int = m.getOrElse(c, 0)
      m(c) = count + 1
    }
    m
  }

  def countChars(s1: String, s2: String): Boolean = {
    val m1     = fillMap(s1)
    val m2     = fillMap(s2)
    var result = true
    for {
      (k, v1) <- m1
    } {
      val v2 = m2(k)
      if (v1 != v2) result = false
    }
    result
  }

  def anagram_checker(string1: String, string2: String): Boolean = {
    if (string1.length != string2.length) false
    else countChars(string1, string2)
  }

  def main(args: Array[String]): Unit = {
    println(s"Not Implemented")
  }
}
