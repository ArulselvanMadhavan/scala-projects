import scala.scalanative.unsafe._
import scala.scalanative.libc._

object Main {
  def main(args: Array[String]):Unit = {
    stdio.printf(c"Hello from native %s\n", c"world")
  }
}
