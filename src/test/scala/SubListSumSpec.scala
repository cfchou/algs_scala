import org.scalacheck.Gen
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.WordSpec

/**
 * User: Chifeng.Chou
 * Date: 04/08/13
 * Time: 10:37
 */
import SubListSum._
object SubListSumSpec {
  val arr1 = Array(21, 1, 7, 5, 9)
  val arr2 = Array(3, 1)
  val arr3 = Array(13, 1, 10, 15, 11, 10, 4, 3)

  val arr4 = Array(13, 1, 10, -8, 15, 11, 10, 4, 3)
}

class SubListSumSpec
  extends WordSpec
  with GeneratorDrivenPropertyChecks
  with ShouldMatchers {

  import SubListSumSpec._

  "test of subListSum of " + arr1.toString + " equals 22 " should {
    subListSum(arr1, 22) should be (true)
  }

  "test of subListSum of " + arr2.toString + " equals 0 " should {
    subListSum(arr2, 0) should be (true)
  }

  "test of subListSum of " + arr3.toString + " equals 0 " should {
    subListSum(arr3, 0) should be (true)
  }

  // ===================================================

  "test against random sublist and its sum" should {
    val g: Gen[List[Int]] = Gen.posNum[Int] flatMap { sz =>
      Gen.listOfN(sz, Gen.posNum[Int])
    }

    def pickAndSum(garr: Gen[List[Int]]): Gen[(Int, List[Int])] = {
      garr flatMap { arr =>
        val sz = arr.size
        Gen.choose(1, sz) flatMap { n =>
          // pick n elements
          Gen.pick(n, arr) flatMap { elems =>
            (elems.sum, arr)
          }
        }
      }
    }

    forAll (pickAndSum(g)) { data =>
      val ret = subListSum(data._2.toIndexedSeq, data._1)
      ret should be (true)
    }
  }

  // ===================================================
  "test of subListSumZero of " + arr3.toString should {
    subListSumZero(arr3) should be (false)
  }

  "test of subListSumZero of " + arr4.toString should {
    subListSumZero(arr4) should be (true)
  }
}
