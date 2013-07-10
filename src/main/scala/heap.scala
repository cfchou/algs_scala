package main

import scala.reflect.ClassTag

/**
 * User: Chifeng.Chou
 * Date: 09/07/13
 * Time: 18:06
 */
object heap {
  trait MaxHeap[T, CC <: MaxHeap[T, CC]] {
    def insert(e: T): Unit
    def max: T
    def delMax: T
    def isEmpty: Boolean = this.size == 0
    def size: Int
  }

  import utili._
  import Ordering.Implicits._  // provide '>', '<'
  // Array-based implementation
  class ArrMaxHeap[T](initSize: Int)(implicit ev: Ordering[T], tag: ClassTag[T])
    extends MaxHeap[T, ArrMaxHeap[T]] {

    var arr = new Array[T](initSize)
    var size = 0
    def this()(implicit ev: Ordering[T], tag: ClassTag[T]) = this(5)

    private def swim[T: Ordering] (arr: Array[T], i: Int) {
      if (i != 0) {
        val p = pIdx(i)
        if (arr(p) < arr(i)) {
          exch(arr, p, i)
          swim(arr, p)
        }
      }
    }

    private def sink[T: Ordering] (arr: Array[T], i: Int) {
      val li = lIdx(i)
      val ri = rIdx(i)
      if (li < size) {
        if (arr(i) < arr(li)) {
          var s = li
          if (ri < size && arr(i) < arr(ri) && arr(li) < arr(ri)) {
            s = ri
          }
          exch(arr, s, i)
          sink(arr, s)
        } else if (ri < size && arr(i) < arr(ri)) {
          exch(arr, ri, i)
          sink(arr, ri)
        }
      }
    }

    // insert to the end, then let it swim up
    def insert(e: T) {
      if (size + 1 > arr.size) {
        arr = arr ++ (new Array[T](arr.size))
      }
      size = size + 1
      arr(size - 1) = e
      swim(arr, size - 1)
    }

    def max: T = arr(0)

    // move the end to the vacancy of the removed head, then let it sink
    def delMax: T = {
      if (isEmpty) throw new IndexOutOfBoundsException
      val rt = max
      arr(0) = arr(size - 1)
      size = size - 1
      sink(arr, 0)
      rt
    }
  }

}
