package dp
//2,1,1,2
//Sub Problems - [2,1,1] + [2]
object HouseRobber {

  private[this] def robNext(nums: Array[Int])(acc: Array[Int], idx: Int): Array[Int] = {
    val value      = nums(idx)
    val stealing   = if(idx - 2 >=0) acc(idx - 2) + value else value
    val noStealing = if(idx - 1 >=0) acc(idx - 1) else value
    acc(idx) = Seq(stealing, noStealing).max
    acc
  }

  def rob(nums: Array[Int]): Int = {
    if (nums.isEmpty) 0
    else {
      val a = new Array[Int](nums.length)
      val result = (0 until a.length).foldLeft(a)(robNext(nums))
      result.last
    }
  }
}
