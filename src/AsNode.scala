/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 03/07/2013
 */

trait AsNode[+T] {
  def prev: AsNode[T]
  //def value: T
}

object NuNode extends AsNode[Nothing] {
  def prev: AsNode[Nothing] = this
  //val value = Nil
}

case class Node[T](value: T, prev: AsNode[T]) extends AsNode[T]

