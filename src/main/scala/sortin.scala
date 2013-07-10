package main

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 07/07/2013
 */
object sortin {

  /* ----------------------------------------
   * Selection sort, in ascending order
   */
  // 1. def selection_sort[T](src: List[T])(implicit ev: Ordering[T]): List[T]
  // 2. view bound, there's an implicit conversion(T => Comparable[T]) in Predef
  //    def selection_sort[T <% Comparable[T]](src: List[T]): List[T]
  // 3. context bound, translated to 1.
  //    def selection_sort[T: Ordering](src: List[T]): List[T]

  import Ordering.Implicits._  // needed for 1 and 3 to provide ">"
  def selection_sort[T: Ordering](src: List[T]): List[T] = {
    if (src.isEmpty) List.empty[T]
    else {
      val x::xs = select[T]((a, b) => a < b)(src) // ascending order
      x::selection_sort(xs)
    }
  }

  // tail-recursive version
  def selection_sort_tr[T: Ordering](src: List[T]): List[T] = {
    def tr[T: Ordering](src: List[T], rev_sorted: List[T]): List[T] = {
      if (src.isEmpty) rev_sorted
      else {
        val x::xs = select[T]((a, b) => a < b)(src) // ascending order
        tr(xs, x::rev_sorted)
      }
    }
    tr(src, List.empty[T]).reverse
  }

  // y::ys where ys.all(pred(y, _))
  private def select[T](pred: (T, T) => Boolean)(xs: List[T]): List[T] = {
    if (xs.isEmpty) List.empty[T]
    else {
      val (ys, y) = xs.tail.foldLeft(List.empty[T], xs.head) { (a, b) =>
        if (pred(a._2, b)) (b::a._1, a._2)
        else (a._2::a._1, b)
      }
      y::ys
    }
  }

  /* -------------------------------------
  * Insertion sort, in ascending order
  */
  def insertion_sort[T: Ordering](src: List[T]): List[T] = {
    src.foldLeft(List.empty[T]) { (lst, e) =>
      insert[T]((a, b) => a < b)(lst, e)  // ascending order
    }
  }

  // FIXME: it traverses the whole "lst", which might not be necessary.
  private def insert[T](pred: (T, T) => Boolean)(lst: List[T], a: T): List[T] = {
    lst.foldRight(List(a)) { (b, acc) =>
      val h = acc.head
      if (pred(h, b)) h::b::acc.tail
      else b::acc
    }
  }

  /* -------------------------------------
  * Uniformly randomly shuffle items in the given list.
  * Note: there's an API util.Random.shuffle
  * FIXME: '++' in the last line makes shuffleTo non-linear.
  */
  def shuffleTo[T](src: List[T]): List[T] = {
    import util.Random
    src.foldLeft(List.empty[T]) { (lst, e) =>
      // a generator with a very much likely different seed every time
      val r = new Random
      // could split at 0, 1, ..., lst.size
      val (xs, ys) = lst.splitAt(r.nextInt(lst.size + 1))
      xs ++ (e::ys)
    }
  }

  /* -------------------------------------
  * Merge sort, in ascending order
  */
  def merge_sort[T: Ordering](src: List[T]): List[T] = {
    if (src.size <= 1) src
    else {
      val (l, r) = src.splitAt(src.size / 2)
      merge(merge_sort(l), merge_sort(r))
    }
  }

  def merge[T: Ordering](lhs: List[T], rhs: List[T]): List[T] = {
    def mrg(xs: List[T], ys: List[T], zs: List[T]): List[T] = {
      if (xs.isEmpty) ys.reverse ++ zs
      else if (ys.isEmpty) xs.reverse ++ zs
      else {
        val x = xs.head
        val y = ys.head
        if (x < y) mrg(xs.tail, ys, x::zs)
        else mrg(xs, ys.tail, y::zs)
      }
    }

    mrg(lhs, rhs, List.empty[T]).reverse
  }



  /* -------------------------------------
  * Quick sort, in ascending order
  */
  def quick_sort[T: Ordering](src: List[T]): List[T] = {

    // Median-of-three partitioning
    // Assume lst.size >= 3
    def partition(lst: List[T]): (List[T], List[T]) = {
      val p =  insertion_sort(lst.take(3)).apply(1)
      lst.foldLeft (List.empty[T], List.empty[T]) { (part, e) =>
        if (e < p) (e::part._1, part._2)
        else (part._1, e::part._2)
      }
    }

    // FIXME: '++' is very inefficient
    def sort(lst: List[T]): List[T] = {
      // improve performance by insertion sort on small lists
      if (lst.size <= 5) insertion_sort(lst)
      else {
        val (lhs, rhs) = partition(lst)
        sort(lhs) ++ (rhs.head :: sort(rhs.tail))
      }
    }

    val good_src = util.Random.shuffle(src)
    sort(good_src)
  }



  /* 3-way quick sort
  *  good: fast, in-place
  *  bad: impure
  */

  // 1. ClassManifest is obsolete
  //    import reflect.ClassManifest
  //    def quick_sort_3w[T: Ordering](src: List[T])(implicit m: ClassManifest[T]): List[T]
  // 2. can't have context/view bounds with implicit list at the same time

  import reflect.ClassTag
  def quick_sort_3w[T](src: List[T])(implicit ev: Ordering[T], tag: ClassTag[T]): List[T] = {

    def sort(arr: Array[T], lo: Int, hi: Int): Array[T] = {
      if (lo < hi) {
        val (le, ge) = partition_3w(arr, lo, hi)
        if (le > lo) sort(arr, lo, le - 1)
        if (ge < hi) sort(arr, ge + 1, hi)
      }
      arr
    }

    val good_src = util.Random.shuffle(src)
    val arr = good_src.toArray[T]
    sort(arr, 0, arr.size - 1).toList
  }

  def quick_sort_3w_rev[T](src: List[T])(implicit ev: Ordering[T], tag: ClassTag[T]): List[T] = {
    quick_sort_3w(src: List[T])(ev.reverse, tag)
  }

  /* -------------------------------------
   * impure helpers
  */
  import utili._

  // [lo, le), [le, ge], (ge, hi]
  def partition_3w[T](arr: Array[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): (Int, Int) = {
    val p = arr(lo)
    var le = lo
    var ge = hi
    var i = le + 1
    while (i <= ge) {
      if (ord.lt(arr(i), p)) {
        exch(arr, i, le)
        le = le + 1
      } else if (ord.gt(arr(i), p)) {
        exch(arr, i, ge)
        ge = ge - 1
      } else {
        i = i + 1
      }
    }
    (le, ge)
  }
}
