package main
/**
 * User: Chifeng.Chou
 * Date: 10/07/13
 * Time: 12:37
 */
object utili {

  // array indexing in the form of a tree
  // note it may not be bound by the size of tree
  def lIdx(idx: Int) = 2 * (idx + 1) - 1
  def rIdx(idx: Int) = lIdx(idx) + 1

  // get parent's index, given @idx is not the root
  def pIdx(idx: Int) = (idx + 1) / 2 - 1


  def exch[T] (arr: Array[T], i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }



}
