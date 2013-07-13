package test
import main.heap.{ArrIndexMinHeap, ArrMaxHeap}
import main.sortin._
import org.scalatest.FunSuite
import org.scalatest.prop.{PropertyChecks, Checkers}

/**
 * User: Chifeng.Chou
 * Date: 10/07/13
 * Time: 12:50
 */
class heapTest extends FunSuite with Checkers {
  test("MaxHeap sorts elements") {
    def inOrder = { arr: List[Int] =>
      val h = new ArrMaxHeap[Int]()
      val sorted = quick_sort_3w(arr)
      for (x <- arr) {
        h.insert(x)
      }
      // Elements inside MaxHeap are in descending order.
      // Prepending next max in MaxHeap to a list creating an ascending list.
      val sorted_asc = (1 to h.size).foldLeft(List.empty[Int]) { (lst, _) =>
        h.delMax::lst
      }
      sorted_asc == sorted
    }
    check(inOrder)
  }
}
