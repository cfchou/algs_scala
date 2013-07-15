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


  // Being able to find/update/delete an element in O(1) based on its key.
  trait IndexMinHeap[T, CC <: IndexMinHeap[T, CC]] {
    implicit protected val cmp: Ordering[T]

    // [0, maxKey] are allowed when insert
    def maxKey: Int
    def incMaxKeyTo(n: Int): Unit

    def insert(key: Int, e: T): Unit
    def min: (Int, T)  // O(1)
    def delMin: (Int, T) // O(log N)

    def isEmpty: Boolean = this.size == 0
    def size: Int

    // if there's an element associated with @key, O(1)
    def contains(key: Int): Boolean

    // query element associated with @key, O(1)
    // assert(contains(key))
    def query(key: Int): T

    // change element associated with @key to @e, O(log N)
    // assert(contains(key))
    def change(key: Int, e: T): Unit

    // delete the element associated with @key, O(log N)
    // no effect if !contain(key)
    def delete(key: Int): Unit
  }

  class ArrIndexMinHeap[T](initSize: Int)
                          (implicit override protected val cmp: Ordering[T],
                           tagT: ClassTag[T])
    extends IndexMinHeap[T, ArrIndexMinHeap[T]] {

    def this()(implicit cmp: Ordering[T], tagT: ClassTag[T]) = this(5)

    // stores (key, item) tuple
    var arr = new Array[(Int, T)](initSize)

    // karr is indexed by key and refers to the index of the corresponding
    // element in arr. '-1' means no element is associated.
    //var karr = new Array[Int](initSize)(-1)
    var karr = Array.fill[Int](initSize)(-1)
    var size = 0


    def maxKey = karr.size - 1 // karr.size == arr.size
    def incMaxKeyTo(newMax: Int): Unit = {
      val inc = newMax - maxKey
      if (inc < 0) throw new IllegalArgumentException
      arr = arr ++ (new Array[(Int, T)](inc))
      karr = karr ++ (Array.fill[Int](inc)(-1))
    }

    private def swim(i: Int): Unit = {
      if (i > 0) {
        val p = pIdx(i)
        if (arr(i)._2 < arr(p)._2) {
          exch(arr, i, p)
          exch(karr, arr(i)._1, arr(p)._1)
        }
        swim(p)
      }
    }

    private def sink(i: Int): Unit = {
      val li = lIdx(i)
      val ri = rIdx(i)
      if (li < size && arr(i)._2 > arr(li)._2) {
        var s = li
        if (ri < size && arr(li)._2 > arr(ri)._2) {
          s = ri
        }
        exch(arr, i, s)
        exch(karr, arr(i)._1, arr(s)._1)
        sink(s)
      } else if (ri < size && arr(i)._2 > arr(ri)._2) {
        exch(arr, i, ri)
        exch(karr, arr(i)._1, arr(ri)._1)
        sink(ri)
      }
    }

    private def swimOrSink(i: Int) {
      if (i > 0) {
        if (arr(pIdx(i))._2 > arr(i)._2) swim(i)
        else sink(i)
      } else { // i == 0
        sink(0)
      }
    }

    /*
    Before insert should do:
    if (k > maxKey) incMaxKeyBy(32)
     */
    def insert(key: Int, e: T) {
      if (contains(key)) {
        change(key, e)
      } else {
        if (key > maxKey) {
          throw new IllegalArgumentException
        }
        karr(key) = size
        arr(size) = (key, e)
        size = size + 1
        swim(size - 1)
      }
    }

    def min: (Int, T) = arr(0)

    // O(1)
    def delMin: (Int, T) = {
      val rt = min
      arr(0) = arr(size - 1)
      karr(rt._1) = -1
      karr(arr(0)._1) = 0
      size = size - 1
      sink(0)
      rt
    }

    // if there's an element associated with @key, O(1)
    def contains(key: Int): Boolean = key <= maxKey && karr(key) != -1

    // query element associated with @key, O(1)
    def query(key: Int): T =
      if (!contains(key)) throw new IllegalArgumentException
      else arr(karr(key))._2

    // change element associated with @key to @e, O(log N)
    def change(key: Int, e: T): Unit = {
      if (!contains(key)) throw new IllegalArgumentException
      else {
        val i = karr(key)
        arr(i) = (key, e)
        swimOrSink(i)
      }
    }

    // delete the element associated with @key, O(log N)
    def delete(key: Int): Unit = {
      if (contains(key)) {
        val t = karr(key)
        karr(key) = -1
        karr(arr(size - 1)._1) = t
        arr(t) = arr(size - 1)

        size = size - 1
        swimOrSink(t)
      }
    }
  }

}
