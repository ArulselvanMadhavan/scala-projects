package clrs.chapter4.exercises
import scala.reflect.ClassTag

object MaxSubArrayLinear {

def helper[T:Numeric](a:Array[T])(acc:Array[(Int, Int, T)], idx:Int):Array[(Int, Int, T)] = {
  val numericInstance = implicitly[Numeric[T]]
  val profit = numericInstance.minus(a(idx), a(idx - 1))
  val (start, end, prevProfit) = acc(idx - 1)
  val cost = numericInstance.negate(a(idx))
  val totalProfit = numericInstance.plus(profit, prevProfit)
  if(numericInstance.gt(cost,totalProfit)) acc(idx) = (idx, idx, cost)
  else acc(idx) = (start, idx, totalProfit)
  acc
}


  def maxSubArray[T:Numeric:ClassTag](a:Array[T]):(Int, Int, T) = {
    val result = new Array[(Int, Int, T)](a.length)
    result(0) = (0,0, implicitly[Numeric[T]].negate(a(0)))
    (1 until a.length).foldLeft(result)(helper(a))
    val sums = result.map(x => x._3)
    val resIdx = sums.indices.maxBy(sums)
    val (start, end, cumSum) = result(resIdx)
    val profit = implicitly[Numeric[T]].plus(a(start), cumSum)
    (start, end, profit)
  }
}
