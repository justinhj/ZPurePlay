object Solution extends App {

  def findSum2(n1: Int, n2: Int, n3s: Set[Int], nums: Array[Int]): Set[List[Int]] = {
    val b = Set.newBuilder[List[Int]]
    n3s.foreach {
      n3: Int =>
        //println(s"nums $n1 $n2 $n3 ${nums(n1) + nums(n2) + nums(n3)}")
        if(nums(n1) + nums(n2) + nums(n3) == 0) b.addOne(List(nums(n1),nums(n2),nums(n3)).sorted)
    }
    b.result()
  }

  def findSum1(n1: Int, n2s: Set[Int],nums: Array[Int]): Set[List[Int]] = {
    val b = Set.newBuilder[List[Int]]
    n2s.foreach {
      n2: Int =>
        b.addAll(findSum2(n1,n2,n2s - n2,nums))
    }
    b.result()
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val d = (0 until nums.size).toSet

    val b = Set.newBuilder[List[Int]]

    d.foreach {
      n1: Int =>
        b.addAll(findSum1(n1,d - n1,nums))
    }

    b.result().toList.reverse
  }

  println(s"3sum ${threeSum(Array(-1,0,1,2,-1,-4))}")
}