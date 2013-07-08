package main

/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 07/07/2013
 */
object sortin {

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

  // 1. def selection[T](src: List[T])(implicit ev: Ordering[T]): List[T]
  // 2. view bound, there's an implicit conversion(T => Comparable[T]) in Predef
  //    def selection[T <% Comparable[T]](src: List[T]): List[T]
  // 3. context bound, translated to 1.
  //    def selection[T: Ordering](src: List[T]): List[T]

  import Ordering.Implicits._  // needed for 1 and 3 to provide ">"
  def selection[T: Ordering](src: List[T]): List[T] = {
    if (src.isEmpty) List.empty[T]
    else {
      val x::xs = select[T]((a, b) => a < b)(src)
      x::selection(xs)
    }
  }

  // tail-recursive version
  def selection_tr[T: Ordering](src: List[T]): List[T] = {
    def tr[T: Ordering](src: List[T], rev_sorted: List[T]): List[T] = {
      if (src.isEmpty) rev_sorted
      else {
        val x::xs = select[T]((a, b) => a > b)(src)
        tr(xs, x::rev_sorted)
      }
    }
    tr(src, List.empty[T]).reverse
  }

}
