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
    assert(dst === selection(src))
    assert(dst === selection_tr(src))
  }

  test("Insertion sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === insertion(src))
  }

  test("Merge sort:") {
    import sortin._
    val src = List(5, 4, 3, 2, 1)
    val dst = src.reverse
    assert(dst === merge_sort(src))
  }



}
