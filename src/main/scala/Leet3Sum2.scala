object Leet3Sum2 extends App {

  import scala.collection.mutable._
  import java.util.Arrays;

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    Arrays.sort(nums)


    val indexBuilder = Map.newBuilder[Int,Int]
    var ib = 0
    while(ib < nums.length) {
      indexBuilder.addOne(nums(ib),ib)
      ib += 1
    }
    val index = indexBuilder.result();

    var r = List.newBuilder[List[Int]]

    var i = 0
    while(i < nums.length - 2) {
      var j = i+1
      while(j < nums.length - 1) {
        val target: Int = 0 - nums(i) - nums(j)

        val lookup = index.getOrElse(target, -1)
        if (lookup > j) {
          r = r.addOne(List(nums(i), nums(j), target))
          j = index(nums(j))
        }
        i = index(nums(i))
        j+=1
      }
      i+=1
    }
    r.result()
  }

  println(s"3sum ${threeSum(Array(-1,0,1,2,-1,-4))}")

}
