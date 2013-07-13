package test

import main.heap.ArrIndexMinHeap
import main.sortin._

import org.scalatest.FunSuite
import org.scalatest.prop.{GeneratorDrivenPropertyChecks}
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Gen

//import org.scalacheck.Arbitrary

/**
 * User: Chifeng.Chou
 * Date: 12/07/13
 * Time: 12:48
 */
class indexHeapTest extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {

  test("test1") {
    forAll { (n: Int) =>
      whenever (n > 1) { (n / 2) should be > 0 }
    }
  }

  test("test2") {
    val M = 10
    val a = List((9,1), (6,-1), (10,-3), (5,-8), (10,-4), (4,-7), (9,-8), (8,-5))
    val h = new ArrIndexMinHeap[Int](M + 1)
    for ((k, e) <- a) {
      h.insert(k, e)
    }
    val sorted_des = (1 to h.size).foldLeft(List.empty[Int]) { (lst, _) =>
      h.delMin._2::lst
    }
    val sorted = sorted_des.sorted.reverse
    sorted_des should equal (sorted)
  }


  test("IndexMinHeap sorts elements, with key [0, 10]") {
    val M = 10
    def genTuple = {
      for { e <- Gen.choose(-1000, 1000)
            k <- Gen.oneOf(0 to M)
      } yield (k, e)
    }
    //def listGen = Gen.listOfN(10, genTuple)
    def listGen = Gen.listOf(genTuple)

    forAll (listGen) { arr: List[(Int, Int)] =>
      val h = new ArrIndexMinHeap[Int](M + 1)
      for ((k, e) <- arr) {
        h.insert(k, e)
      }
      val sorted_des = (1 to h.size).foldLeft(List.empty[Int]) { (lst, _) =>
        h.delMin._2::lst
      }
      val sorted = quick_sort_3w_rev(sorted_des)
      sorted_des should equal (sorted)
    }
  }
}
