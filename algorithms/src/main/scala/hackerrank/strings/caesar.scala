package hackerrank.strings
import java.util.Scanner;

object Caesar {

  private[this] def rotateAscii(k: Int, start: Int, c: Int): Char = {
    val ascii = (c.toInt - start + k)
    val extra = ascii % 26
    (extra + start).toChar
  }

  private[this] def rotateLetter(k: Int, c: Char): Char = {
    if (c.isLower)
      rotateAscii(k, 97, c.toInt)
    else rotateAscii(k, 65, c.toInt)
  }

  private[this] def rotate(k: Int)(c: Char): Char = {
    if (c.isLetter)
      rotateLetter(k, c)
    else c
  }

  def cipher(k: Int, s: String): String = {
    s.map(rotate(k)).toString
  }

  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in);
    sc.nextInt()
    val s = sc.next()
    val k = sc.nextInt()
    println(cipher(k, s))
  }
}
