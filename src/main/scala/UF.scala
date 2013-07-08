package main
/**
 * User: Chifeng.Chou
 * Date: 05/07/13
 * Time: 11:29
 */
trait UF[CC <: UF[CC]] {
  def union(p: Int, q: Int): CC
  def find(p: Int): Int
  def connected(p: Int, q: Int): Boolean = find(p) == find(q)
  def count: Int
}


class QuickFind(val size: Int) extends UF[QuickFind] {
  var arr = {
    val a = new Array[Int](size + 1)
    (0 to size).map { i => a(i) = i }
    a
  }

  var count = size

  def find(p: Int) = {
    if (p > size) throw new IndexOutOfBoundsException
    else arr(p)
  }
  def union(p: Int, q: Int) = {
    val c1 = find(p)
    val c2 = find(q)
    arr = if (c1 < c2) {
      arr.map { c =>
        if (c == c2) {
          count = count - 1
          c1
        }
        else c
      }
    } else  if (c1 > c2) {
      arr.map { c =>
        if (c == c1) {
          count = count - 1
          c2
        }
        else c
      }
    } else arr
    this
  }
}

class QuickUnion(val size: Int) extends UF[QuickUnion] {
  var arr = {
    val a = new Array[Int](size + 1)
    (0 to size).map { i => a(i) = i }
    a
  }
  var weights = {
    val a = new Array[Int](size)
    (0 to size + 1).map { i => a(i) = 1 }
    a
  }

  var count = size

  def find(p: Int) = {
    val parent = arr(p)
    if (parent != p) find(parent)
    else parent
  }

  def union(p: Int, q: Int) = {
    val c1 = find(p)
    val c2 = find(q)
    val w1 = weights(c1)
    val w2 = weights(c2)
    arr = if (w1 < w2) {
      weights(c1) = w1 + w2
      arr(c2) = c1
      arr
    } else if (w1 > w2) {
      weights(c2) = w1 + w2
      arr(c1) = c2
      arr
    } else arr
    this
  }
}









