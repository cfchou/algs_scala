package test
import main._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * User: Chifeng.Chou
 * Date: 05/07/13
 * Time: 16:09
 */

class testSuite extends FunSuite with Checkers {
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

  /* -------------------------------
  Property-based tests
  ScalaCheck style
   */

  // The 3 partitions from @arr should be >, = , < @arr(0) respectively.
  test("3-way partition property check:") {
    import sortin._
    def validRange = { (arr: Array[Int]) =>
      if (!arr.isEmpty) {
        val p = arr(0)
        val (le, ge) = partition_3w(arr, 0, arr.size - 1)
        val (a, b) = arr.splitAt(le)
        var (c, d) = b.splitAt(ge - le + 1)
        a.forall(_ < p) && c.forall(_ == p) && d.forall(_ > p)
      } else true
    }
    check(validRange)
  }

  // Given a List @lst, the ascending-ordered @lst should be the reverse of
  // the descending ordered @lst.
  test("3-way Quick sort property check:") {
    import sortin._
    def isReverse = { (arr: List[Int]) =>
      val a = quick_sort_3w(arr)
      val b = quick_sort_3w_rev(arr)
      a == b.reverse
    }
    check(isReverse)
  }
}
