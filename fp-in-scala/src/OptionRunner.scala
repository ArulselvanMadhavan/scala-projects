/**
  * Created by amadhavan1 on 7/23/17.
  */

import fpinscala.errorhandling.Option
import fpinscala.errorhandling.Some
import fpinscala.errorhandling.None

object OptionRunner {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m: Option[Double] = mean(xs)
    m.flatMap((mn: Double) => mean(xs.map((x: Double) => math.pow(x - mn, 2))))
  }


  def main(args: Array[String]): Unit = {
    val o1: Option[Int] = Some(4)
    val o2 = List(2.0, 4.0, 8.0, 32.0)
    val o3: List[Option[Double]] = List(Some(1.0), Some(3.0), Some(5.0), Some(15.0))
    println("Mean %s".format(mean(o2)))
    println("Variance %s".format(variance(o2)))
    println(o1.filter((x: Int) => (x % 2 == 0)))
    println(Option.sequence(o3))
    println(Option.sequence4(o3))
  }
}
