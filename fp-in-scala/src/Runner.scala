/**
  * Created by amadhavan1 on 7/3/17.
  */
object Runner {

  def abs(n:Int):Int =
    if(n < 0) -n
    else n

  private def formatAbs(x:Int):String = {
    val msg = "The absolute value of %d is %d";
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println("FP In Scala Runner");
    println(formatAbs(-42));
  }
}
