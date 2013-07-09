package test
import main._
import org.scalatest.FunSuite
/**
 * User: Chifeng.Chou
 * Date: 05/07/13
 * Time: 16:09
 */

class testSuite extends FunSuite {
  test("QuickFind: before any union") {
    val QF_SIZE = 10
    val uf = new QuickFind(QF_SIZE)
    val bs = for {
      i <- (1 to 10)
      if i == uf.find(i)
    } yield i
    assert(bs.size == QF_SIZE)
  }

  test("Selection sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === selection_sort(src))
    assert(dst === selection_sort_tr(src))
  }

  test("Insertion sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === insertion_sort(src))
  }

  test("Merge sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === merge_sort(src))
  }

  test("Quick sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === quick_sort(src))
  }

  test("3-way partition 1:") {
    import sortin._
    val src = Array(3, 4, 5, 2, 1)
    assert((2, 2) === partition_3w(src, 0, src.size - 1))
  }

  test("3-way partition 2:") {
    import sortin._
    val src = Array(1, 1, 1, 1, 1)
    assert((0, 4) === partition_3w(src, 0, src.size - 1))
  }

  test("3-way partition 3:") {
    import sortin._
    val src = Array(5, 4, 3, 2, 1)
    assert((4, 4) === partition_3w(src, 0, src.size - 1))
  }

  test("3-way partition 4:") {
    import sortin._
    val src = Array(1, 2, 3, 4, 5)
    assert((0, 0) === partition_3w(src, 0, src.size - 1))
  }

  test("3-way partition 5:") {
    import sortin._
    val src = Array(4, 2, 3, 4, 5)
    assert((2, 3) === partition_3w(src, 0, src.size - 1))
  }

  test("3-way Quick sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === quick_sort_3w(src))
  }

  test("3-way Quick sort rev:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(src === quick_sort_3w_rev(dst))
  }
}
